#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifndef WIN32
# include <unistd.h>
# include <signal.h>
#endif

void R_init_pbmcapply(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

SEXP setpgid_(SEXP x_) {
  int res = 0;

  #ifndef WIN32
    int id = Rf_asInteger(x_);
    if (setpgid(id, id) == 1) res = 1;
  #else
    Rf_warning("set group process id is not supported on this platform");
  #endif

  return Rf_ScalarLogical(res);
}

SEXP killp_(SEXP pgid_) {
  int res = 0;

  #ifndef WIN32
    int id = Rf_asInteger(pgid_);
    if (killpg(id, SIGTERM) == 0) res = 1;
  #else
    Rf_warning("kill process is not supported on this platform");
  #endif

  return Rf_ScalarLogical(res);
}
