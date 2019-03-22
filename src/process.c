#include <R.h>
#include <Rinternals.h>
#ifndef Win32
# include <unistd.h>
# include <signal.h>
#endif

SEXP setpgid_(SEXP x_) {
  int id = asInteger(x_);
  int res = 0;
#ifndef Win32
  if (setpgid(id, id) == 1) res = 1;
#else
  warning(_("set group process id is not supported on this platform"));
#endif
  return ScalarLogical(res);
}

SEXP killp_(SEXP pgid_) {
  int id = asInteger(pgid_);
  int res = 0;
#ifndef Win32
  if (killpg(id, SIGTERM) == 0) res = 1;
#else
  warning(_("kill process is not supported on this platform"));
#endif
  return ScalarLogical(res);
}
