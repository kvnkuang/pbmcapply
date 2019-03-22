#include <R.h>
#include <Rinternals.h>
#include <unistd.h>
#include <signal.h>

SEXP setpgid_(SEXP x_) {
  int id = asInteger(x_);
  int res = 0;
  if (setpgid(id, id) == 1) res = 1;
  return ScalarLogical(res);
}

SEXP killp_(SEXP pgid_) {
  int id = asInteger(pgid_);
  int res = 0;
  if (killpg(id, SIGTERM) == 0) res = 1;
  return ScalarLogical(res);
}
