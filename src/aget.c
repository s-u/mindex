#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <Rinternals.h>

static void R_unmap(SEXP s) {
    if (TYPEOF(s) == EXTPTRSXP && TYPEOF(EXTPTR_PROT(s)) == REALSXP) {
	char *mem = (char*) EXTPTR_PTR(s);
	size_t len = (size_t) REAL(EXTPTR_PROT(s))[0];
	munmap(mem, len);
    }
}

static const char* map__(SEXP sFN, size_t *sz) {
    int fd;
    const char *fn;
    const char *mem;
    struct stat st;

    if (TYPEOF(sFN) != STRSXP || LENGTH(sFN) != 1)
	Rf_error("invalid filename");
    fn = CHAR(STRING_ELT(sFN, 0));
    fd = open(fn, O_RDONLY);
    if (fd < 0)
	Rf_error("Unable to open `%s' for reading", fn);
    if (fstat(fd, &st)) {
	close(fd);
	Rf_error("Unable to stat `%s'", fn);
    }
    if (sz)
	sz[0] = (size_t) st.st_size;
    mem = (const char*) mmap(0, st.st_size, PROT_READ, MAP_FILE | MAP_SHARED, fd, 0);
    close(fd);
    if (!mem)
	Rf_error("Unable to map `%s'", fn);
    return mem;
}

SEXP map_file(SEXP sFN) {
    SEXP res;
    size_t sz;
    const char *mem = map__(sFN, &sz);
    /* we are storing the size in a double, so we're limited to 2^53 sizes but that shoudl be ok for now ... */
    res = PROTECT(R_MakeExternalPtr((void*)mem, R_NilValue, ScalarReal(sz)));
    R_RegisterCFinalizer(res, R_unmap);
    setAttrib(res, R_ClassSymbol, mkString("mmap"));
    UNPROTECT(1);
    return res;
}

SEXP slurp_(SEXP sFN, SEXP sType) {
    SEXP res;
    size_t sz, i;
    char ty = 'i';
    double *d;
    int *iv;
    unsigned long *l;
    const char *mem = map__(sFN, &sz);

    if (TYPEOF(sType) == STRSXP && LENGTH(sType))
	ty = CHAR(STRING_ELT(sType, 0))[0];
    if (TYPEOF(sType) == INTSXP)
	ty = 'i';
    if (TYPEOF(sType) == REALSXP)
	ty = 'd';
    if (TYPEOF(sType) == RAWSXP)
	ty = 'b';

    switch (ty) {
    case 'd':
	d = REAL(res = allocVector(REALSXP, sz / sizeof(double)));
	memcpy(d, mem, sz);
	munmap((void*)mem, sz);
	return res;
    case 'i':
	iv = INTEGER(res = allocVector(INTSXP, sz / sizeof(int)));
	memcpy(iv, mem, sz);
	munmap((void*)mem, sz);
	return res;
    case 'b':
	memcpy(RAW(res = allocVector(RAWSXP, sz)), mem, sz);
	munmap((void*)mem, sz);
	return res;
    case 'l':
	sz /= sizeof(unsigned long);
	l = (unsigned long*) mem;
	d = REAL(res = allocVector(REALSXP, sz));
	for (i = 0; i < sz; i++)
	    d[i] = (double) l[i];
	munmap((void*)mem, sz);
	return res;
    default:
	Rf_error("Invalid type, choose one of 'i', 'd', 'b', 'l'");
    }
    return R_NilValue;
}

SEXP ascii_index(SEXP sMmap, SEXP sIndex, SEXP sSep, SEXP sI, SEXP sJ) {
    int sep = '|';
    const char *mem;
    const int *ix_i;
    const int *J, *I;
    const double *ix_d;
    int iix, nJ, N, iN, maxJ = 0;
    int i, res_i = 0; /* FIXME: long vector support? */
    int *JL;
    SEXP res, sJL, sJS;

    if (!Rf_inherits(sMmap, "mmap")) Rf_error("invalid mmap object");
    mem = (const char*) EXTPTR_PTR(sMmap);
    if (TYPEOF(sIndex) == REALSXP) {
	iix = 0;
	ix_d = REAL(sIndex);
    } else {
	iix = 1;
	ix_i = INTEGER(sIndex);
    }
    iN = LENGTH(sIndex);
    if (TYPEOF(sSep) == STRSXP && LENGTH(sSep) && CHAR(STRING_ELT(sSep, 0))[0])
	sep = (int) (unsigned char) CHAR(STRING_ELT(sSep, 0))[0];
    sJ = PROTECT(coerceVector(sJ, INTSXP));
    sI = PROTECT(coerceVector(sI, INTSXP));
    nJ = LENGTH(sJ);
    J = INTEGER(sJ);
    I = INTEGER(sI);
    N = LENGTH(sI);

    /* find out which fields will be needed */
    for (i = 0; i < nJ; i++)
	if (J[i] > maxJ)
	    maxJ = J[i];
	else if (J[i] < 1)
	    Rf_error("only positive column subscripts are allowed");
    JL = INTEGER(sJL = PROTECT(allocVector(INTSXP, maxJ)));
    for (i = 0; i < nJ; i++)
	JL[J[i] - 1] = 1;
    /* cache */
    sJS = PROTECT(allocVector(STRSXP, maxJ));

    res = PROTECT(allocMatrix(STRSXP, N, nJ));
    for (i = 0; i < N; i++) {
	int j, f = 0;
	/* parse out fields the we need in sequence */
	if (I[i] > 0 && I[i] < iN) {
	    unsigned long i0 = (unsigned long) (iix ? ix_i[I[i] - 1] : ix_d[I[i] - 1]);
	    unsigned long i1 = (unsigned long) (iix ? ix_i[I[i]] : ix_d[I[i]]);
	    /* printf("line: %lu - %lu\n", i0, i1); */
	    const char *c = mem + i0, *e = mem + i1;
	    while (c < e && f < maxJ) {
		const char *ce = memchr(c, sep, e - c);
		if (!ce)
		    ce = e;
		if (JL[f])
		    SET_STRING_ELT(sJS, f, mkCharLenCE(c, ce - c, CE_UTF8));
		f++;
		c = ce + 1;
	    }
	}
	/* printf("found %d of %d fields\n", f, maxJ); */
	/* fill up with NAs */
	while (f < maxJ) {
	    if (STRING_ELT(sJS, f) != R_NaString) SET_STRING_ELT(sJS, f, R_NaString);
	    f++;
	}
	/* set all the columns */
	res_i = i;
	for (j = 0; j < nJ; j++) {
	    SET_STRING_ELT(res, res_i, STRING_ELT(sJS, J[j] - 1));
	    res_i += N;
	}
    }
    UNPROTECT(5);
    return res;
}
