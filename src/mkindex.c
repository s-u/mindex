/* create an index into an ASCII file */
#include <stdio.h>
#include <string.h>

#include <Rinternals.h>

static char buf[1024*1024];

SEXP mkindex(SEXP sFN, SEXP sIFN) {
    const char *ifn = CHAR(STRING_ELT(sFN, 0));
    const char *ofn = CHAR(STRING_ELT(sIFN, 0));
    FILE *f, *out;
    unsigned long l = 0;

    f = fopen(ifn, "rb");
    if (!f) Rf_error("ERROR: cannot open `%s'", ifn);
    out = fopen(ofn, "wb");
    if (!out) {
	fclose(f);
	Rf_error("ERROR: cannot create `%s'", ofn);
    }

    while (!feof(f) && fgets(buf, sizeof(buf), f)) {
	fwrite(&l, sizeof(l), 1, out);
	l += strlen(buf);
    }
    fwrite(&l, sizeof(l), 1, out);
    fclose(f);
    fclose(out);
    return ScalarReal(l);
}
