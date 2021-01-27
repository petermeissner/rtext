#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _rtext_which_token_worker(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_rtext_which_token_worker", (DL_FUNC) &_rtext_which_token_worker, 3},
    {NULL, NULL, 0}
};

void R_init_rtext(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
