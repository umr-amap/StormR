//stormBehaviour.cpp

#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List
computeAsymmetry_cpp(
    String asymmetry,
    NumericVector wind,
    NumericVector x,
    NumericVector y,
    double vx,
    double vy,
    double vh,
    double r,
    double rmw,
    double lat) {
  double pi = 3.141592653589793238463 ;
  int s = wind.size();
  NumericVector dir = -(atan(y / x) - pi / 2);
  if (lat >= 0) {
    dir = dir - pi / 2;
  } else {
    dir = dir + pi / 2;
  }

  for(int i = 0; i < s; i++) {
    if (dir[i] < 0) {
      dir[i] += 2 * pi;
    } else if (dir[i] > 2 * pi) {
      dir[i] -= 2 * pi;
    }
  }

  NumericVector windX = wind * cos(dir);
  NumericVector windY = wind * sin(dir);

  double stormDir = -(atan(vy / vx) - pi / 2);
  if (stormDir < 0) {stormDir += 2 * pi;}
  double mWindX = vh * cos(stormDir);
  double mWindY = vh * sin(stormDir);

  double formula = 0;
  if (asymmetry == String::"Chen") {
    formula = 3 * pow(rmw, 3 / 2) * pow(r, 3 / 2) / (pow(rmw, 3) + pow(r, 3) + pow(rmw, 3 / 2) * pow(r, 3 / 2));
  } else if (asymmetry == String::"Miyazaki") {
    formula = exp(-r / 500 * pi);
  }

  NumericVector tWindX = windX + formula * mWindX;
  NumericVector tWindY = windY + formula * mWindY;

  NumericVector twind = sqrt(pow(tWindX, 2) + pow(tWindY, 2));
  NumericVector direction = atan(tWindY / tWindX) * 180 / pi;

  for(int i = 0; i < s; i++) {
    if (direction[i] < 0) {
      direction[i] += 360;
    }
  }

  return List::create(std::round(twind * 1000.0) / 1000.0, std::round(direction * 1000.0) / 1000.0);
}



// [[Rcpp::export]]
NumericVector
willoughby_cpp(NumericVector r,
               double rmw,
               double msw,
               double lat) {
  int s = r.size();
  NumericVector vr(s);
  double x2 = 25;
  double x1 = 287.6 - 1.942 * msw + 7.799 * log(rmw) + 1.819 * abs(lat);
  double a = 0.5913 + 0.0029 * msw - 0.1361 * log(rmw) - 0.0042 * abs(lat);
  double n = 2.1340 + 0.0077 * msw - 0.4522 * log(rmw) - 0.0038 * abs(lat);
  for(int i = 0; i < s; ++i) {
    if (r[i] >= rmw) {
      vr[i] = msw * ((1 - a) * exp(-abs((r[i] - rmw) / x1)) +
        a * exp(-abs(r[i] - rmw) / x2));
    } else {
      vr[i] = msw * abs(pow(r[i] / rmw,n));
    }
    vr[i] = std::round(vr[i]* 1000.0) / 1000.0;
  }
  return vr;
}
