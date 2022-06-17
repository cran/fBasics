#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void gl_fm5_distfunc(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void gl_fmkl_distfunc(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void gl_rs_distfunc(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void pNIG(void *, void *, void *, void *, void *, void *, void *);
extern void qNIG(void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"gl_fm5_distfunc",  (DL_FUNC) &gl_fm5_distfunc,  12},
    {"gl_fmkl_distfunc", (DL_FUNC) &gl_fmkl_distfunc, 11},
    {"gl_rs_distfunc",   (DL_FUNC) &gl_rs_distfunc,   11},
    {"pNIG",             (DL_FUNC) &pNIG,              7},
    {"qNIG",             (DL_FUNC) &qNIG,              7},
    {NULL, NULL, 0}
};

void R_init_fBasics(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
