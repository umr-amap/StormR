#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::vec willoughby_cppa(const arma::vec& r, double rmw, double msw, double lat) {
  double x1 = 287.6 - 1.942 * msw + 7.799 * std::log(rmw) + 1.819 * std::abs(lat);
  double x2 = 25;
  double a = 0.5913 + 0.0029 * msw - 0.1361 * std::log(rmw) - 0.0042 * std::abs(lat);
  double n = 2.1340 + 0.0077 * msw - 0.4522 * std::log(rmw) - 0.0038 * std::abs(lat);

  arma::vec vr = r;
  arma::uvec indices_ge_rmw = find(r >= rmw);
  arma::uvec indices_lt_rmw = find(r < rmw);

  vr.elem(indices_ge_rmw) = msw * ((1 - a) * exp(-abs((r.elem(indices_ge_rmw) - rmw) / x1)) + a * exp(-abs(r.elem(indices_ge_rmw) - rmw) / x2));
  vr.elem(indices_lt_rmw) = msw * pow(abs((r.elem(indices_lt_rmw) / rmw)), n);

  vr = arma::round(vr * 1000.0) / 1000.0;

  return vr;
}

// [[Rcpp::export]]
arma::vec boose_cppa(const arma::vec& r, double rmw, double msw, double pc, double poci, const arma::vec& x, const arma::vec& y, const arma::vec& vx, const arma::vec& vy, double vh, const arma::uvec& landIntersect, double lat) {
  double rho = 1; // air density
  double b = rho * std::exp(1) * std::pow(msw, 2) / (poci - pc);

  arma::vec vr = sqrt(pow((rmw / r), b) * exp(1 - pow((rmw / r), b)));

  arma::vec angle;
  if (lat >= 0) {
    // Northern Hemisphere, t is clockwise
    angle = atan2(vy, vx) - atan2(y, x);
  } else {
    // Southern Hemisphere, t is counterclockwise
    angle = atan2(y, x) - atan2(vy, vx);
  }

  arma::uvec indices_landIntersect_1 = find(landIntersect == 1);
  arma::uvec indices_landIntersect_0 = find(landIntersect == 0);

  vr.elem(indices_landIntersect_1) = 0.8 * (msw - (1 - sin(angle.elem(indices_landIntersect_1))) * vh / 2) * vr.elem(indices_landIntersect_1);
  vr.elem(indices_landIntersect_0) = (msw - (1 - sin(angle.elem(indices_landIntersect_0))) * vh / 2) * vr.elem(indices_landIntersect_0);

  vr = arma::round(vr * 1000.0) / 1000.0;

  return vr;
}