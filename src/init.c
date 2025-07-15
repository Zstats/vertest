#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP call_bytecode_func(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"call_bytecode_func", (DL_FUNC) &call_bytecode_func, 3},
  {NULL, NULL, 0}
};

void R_init_zstats1(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
