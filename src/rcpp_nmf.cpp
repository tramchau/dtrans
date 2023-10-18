#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

// [[Rcpp::depends("RcppArmadillo")]]

arma::mat rcpp_debug(arma::mat x, arma::mat W, arma::mat H, bool update_H, int max_iter) {
  double eps = 2.2204e-16;
  H = H % (W.t() * x)/((W.t() * W) * H + eps);
  
  return(H);
}

// [[Rcpp::export]]
List rcpp_optimize_WH(arma::mat x, arma::mat W, arma::mat H, bool update_H, int max_iter) {
  double eps = 2.2204e-16;
  
  int iter_final = 0;
  double errorx = 0;
  for (int iter=1;  iter <= max_iter; iter++) {
    if (update_H) {
      H = H % (W.t() * x)/((W.t() * W) * H + eps);
    }
    W = W % (H * x.t()).t()/(W * (H * H.t()) + eps);
      
    errorx = mean(mean(abs(x - (W * H))))/mean(mean(x));
    if (errorx < 1e-05) {
      Rprintf("Execution finishes at iteration = $d...", iter);
      iter_final = iter;
      break;
    }
    iter_final = iter;
  }
  arma::mat temp = x - (W * H);
  
  double eucl_dist = sum(sum(temp % temp));
  
  List ret = List::create(Named("W") = W,
                          Named("H") = H,
                          Named("eucl_dist") = eucl_dist,
                          Named("relative_err") = errorx,
                          Named("stop_iter") = iter_final);
  return ret;
}