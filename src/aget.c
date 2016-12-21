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

static const char *bs(const char *mem, size_t left, size_t right, const char *what, size_t kl, size_t rl) {
    while (right > left) {
	size_t mid = (left + right) >> 1;
	const char *midm = mem + mid * rl;
	int c = memcmp(midm, what, kl);
	if (c == 0) return midm;
	if (c < 0) left = mid + 1;
	else right = mid - 1;
    }
    if (left == right && !memcmp(mem + left * rl, what, kl)) return mem + left * rl;
    return 0;
}

SEXP bin_srch(SEXP sMap, SEXP sWhat, SEXP sKeyLen, SEXP sRowLen, SEXP sSepLen, SEXP sTraiLen, SEXP sNA) {
    size_t keylen = asInteger(sKeyLen);
    size_t rowlen = asInteger(sRowLen);
    size_t seplen = asInteger(sSepLen);
    size_t trlen  = asInteger(sTraiLen);
    if (TYPEOF(sWhat) != STRSXP)
	Rf_error("what is expected to be a character vector");
    if (TYPEOF(sMap) != EXTPTRSXP || TYPEOF(EXTPTR_PROT(sMap)) != REALSXP)
	Rf_error("invalid mmap object");
    
    SEXP NAstr = (TYPEOF(sNA) == STRSXP && LENGTH(sNA) == 1) ? STRING_ELT(sNA, 0) : NA_STRING;
    size_t n = XLENGTH(sWhat);
    SEXP res = PROTECT(allocVector(STRSXP, n));
    
    const char *mem = (const char*) EXTPTR_PTR(sMap);
    size_t len = (size_t) REAL(EXTPTR_PROT(sMap))[0];
    size_t i = 0;

    while (i < n) {
	const char *w = CHAR(STRING_ELT(sWhat, i));
	const char *rr = bs(mem, 0, (len / rowlen) - 1, w, keylen, rowlen);
	if (rr)
	    SET_STRING_ELT(res, i, mkCharLen(rr + keylen + seplen, rowlen - keylen - seplen - trlen));
	else
	    SET_STRING_ELT(res, i, NAstr);
	i++;
    }
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

SEXP mkindex(SEXP sFN, SEXP sIFN) {
    const char *ofn = CHAR(STRING_ELT(sIFN, 0));
    FILE *out;
    unsigned long l = 0;
    size_t sz;
    const char *mem = map__(sFN, &sz), *end = mem + sz, *c;
    
    out = fopen(ofn, "wb");
    if (!out) {
        munmap((void*)mem, sz);
        Rf_error("ERROR: cannot create `%s'", ofn);
    }
    c = mem;
    while (c < end) {
	l = (unsigned long) (c - mem);
        fwrite(&l, sizeof(l), 1, out);
	c = memchr(c, '\n', end - c);
	if (!c) break;
	c++;
    }
    l = (unsigned long) (end - mem);
    /* if the trailing newline is missing, add one
       since the next record cannot start here */
    if (end > mem && end[-1] != '\n') l++;
    fwrite(&l, sizeof(l), 1, out);
    fclose(out);
    munmap((void*)mem, sz);
    return ScalarReal(l);
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
    if (!iN) Rf_error("Invalid (empty) index");
    if (TYPEOF(sSep) == STRSXP && LENGTH(sSep) && CHAR(STRING_ELT(sSep, 0))[0])
	sep = (int) (unsigned char) CHAR(STRING_ELT(sSep, 0))[0];
    if (sI == R_NilValue) {
	int *ii = INTEGER(sI = allocVector(INTSXP, iN - 1));
	for (i = 0; i < iN - 1; i++)
	    ii[i] = i + 1;
    }
    sI = PROTECT(coerceVector(sI, INTSXP));
    if (sJ == R_NilValue) {
	if (iN == 1)
	    sJ = allocVector(INTSXP, 0);
	else { /* we have to count the fields in the first line */
	    unsigned long i0 = (unsigned long) (iix ? ix_i[0] : ix_d[0]);
	    unsigned long i1 = (unsigned long) (iix ? ix_i[1] : ix_d[1]);
            const char *c = mem + i0, *e = mem + i1;
	    int fs = 0, *jj;
            while (c < e) {
                const char *ce = memchr(c, sep, e - c);
                if (!ce)
                    ce = e;
                fs++;
                c = ce + 1;
            }
	    jj = INTEGER(sJ = allocVector(INTSXP, fs));
	    for (i = 0; i < fs; i++)
		jj[i] = i + 1;
	}
    }
    sJ = PROTECT(coerceVector(sJ, INTSXP));
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
