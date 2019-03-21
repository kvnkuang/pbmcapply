#include <Rcpp.h>
#include <unistd.h>
#include <signal.h>
using namespace Rcpp;

// [[Rcpp::export]]
int setpgid(int x) {
  int res = setpgid(x, x);
  return res;
}

// [[Rcpp::export]]
int killp(int pgid) {
  int res = killpg(pgid, SIGTERM);
  return res;
}
