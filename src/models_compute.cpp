#include <Rcpp.h>

// =============================================================================
// =====================ccp functions for models ===============================
// =============================================================================

// [[Rcpp::export]]
Rcpp::NumericVector
  willoughby_cpp(Rcpp::NumericVector r,
                 const double rmw,
                 const double msw,
                 const double lat) {
    const int s = r.size();
    Rcpp::NumericVector vr(s);
    double x2 = 25;
    double x1 = 287.6 - 1.942 * msw + 7.799 * log(rmw) + 1.819 * abs(lat);
    double a = 0.5913 + 0.0029 * msw - 0.1361 * log(rmw) - 0.0042 * abs(lat);
    double n = 2.1340 + 0.0077 * msw - 0.4522 * log(rmw) - 0.0038 * abs(lat);

    for(int i = 0; i < s; ++i) {
      double r_ = r[i];
      if (r_ >= rmw) {
        vr[i] = msw * ((1 - a) * exp(-abs((r_ - rmw) / x1)) +
          a * exp(-abs(r_ - rmw) / x2));
      } else {
        vr[i] = msw * abs(pow(r_ / rmw,n));
      }
      vr[i] = std::round(vr[i]* 1000.0) / 1000.0;
    }
    return vr;
  }

// [[Rcpp::export]]
Rcpp::NumericVector
  holland_cpp(Rcpp::NumericVector r,
              const double rmw,
              const double msw,
              const double pc,
              const double poci,
              const double lat){

    const double rho = 1.15; //air density
    const double f = 2 * 7.29 * pow(10,-5) * sin(lat); //Coriolis parameter
    const double b = rho * exp(1) * pow(msw, 2) / (poci - pc);

    const int s = r.size();
    Rcpp::NumericVector vr(s);

    for(int i = 0; i < s; ++i) {
      vr[i] = sqrt(b / rho * pow(rmw / r[i], b) * (poci - pc) * exp(-pow(rmw / r[i], b)) + pow(r[i] * f / 2, 2)) - r[i] * f / 2;
      vr[i] = std::round(vr[i]* 1000.0) / 1000.0;
    }

    return vr;
  }

// [[Rcpp::export]]
Rcpp::NumericVector
  boose_cpp(Rcpp::NumericVector r,
            const double rmw,
            const double msw,
            const double pc,
            const double poci,
            Rcpp::NumericVector x,
            Rcpp::NumericVector y,
            const double vx,
            const double vy,
            const double vh,
            Rcpp::NumericVector landIntersect,
            const double lat){


    const double rho = 1; //air density
    const double b = rho * exp(1) * pow(msw, 2) / (poci - pc);

    const int s = r.size();
    double v;
    Rcpp::NumericVector vr(s);
    double angle(s);

    if(lat>= 0){
      // Northern Hemisphere, t is clockwise
      for(int i = 0; i < s; ++i) {
        angle = atan2(vy, vx) - atan2(y[i], x[i]);
        v = sqrt(pow(rmw / r[i], b) * exp(1 - pow(rmw / r[i], b)));
        if(landIntersect[i] == 1){
          vr[i] = 0.8 * (msw - (1 - sin(angle)) * vh / 2) * v;
        }else{
          vr[i] = (msw - (1 - sin(angle)) * vh / 2) * v;
        }
        vr[i] = std::round(vr[i]* 1000.0) / 1000.0;
      }
    }else{
      // Southern Hemisphere, t is counterclockwise
      for(int i = 0; i < s; ++i) {
        angle = atan2(y[i], x[i]) - atan2(vy, vx);
        v = sqrt(pow(rmw / r[i], b) * exp(1 - pow(rmw / r[i], b)));
        if(landIntersect[i] == 1){
          vr[i] = 0.8 * (msw - (1 - sin(angle)) * vh / 2) * v;
        }else{
          vr[i] = (msw - (1 - sin(angle)) * vh / 2) * v;
        }
        vr[i] = std::round(vr[i]* 1000.0) / 1000.0;
      }
    }

    return vr;
  }


// [[Rcpp::export]]
Rcpp::List
  computeAsymmetry_cpp(
    const Rcpp::String asymmetry,
    const Rcpp::NumericVector wind,
    const Rcpp::NumericVector x,
    const Rcpp::NumericVector y,
    const double vx,
    const double vy,
    const double vh,
    const Rcpp::NumericVector r,
    const double rmw,
    const double lat) {

    const double pi = 3.141592653589793238463;
    const int s = wind.size();
    const Rcpp::String Chen = "Chen";
    const Rcpp::String Miyazaki = "Miyazaki";

    double dir;
    double stormDir = -(atan2(vy, vx) - pi / 2);
    if (stormDir < 0){
      stormDir += 2 * pi;
    }

    double formula;
    double twindX;
    double twindY;

    Rcpp::NumericVector twind(s);
    Rcpp::NumericVector direction(s);

    for(int i = 0; i < s; i++) {
      if (lat >= 0) {
        dir = -(atan2(y[i], x[i]) - pi / 2) - pi / 2;
      } else {
        dir = -(atan2(y[i], x[i]) - pi / 2) + pi / 2;
      }
      if (dir < 0){
        dir += 2 * pi;
      }
      if (dir > 2 * pi){
        dir -= 2 * pi;
      }
      if (asymmetry == Chen) {
        formula = 3 * pow(rmw, 3 / 2) * pow(r[i], 3 / 2) / (pow(rmw, 3) + pow(r[i], 3) + pow(rmw, 3 / 2) * pow(r[i], 3 / 2));
      } else if (asymmetry == Miyazaki) {
        formula = exp(-r[i] / 500 * pi);
      }
      twindX = wind[i] * cos(dir) + formula * vh * cos(stormDir);
      twindY = wind[i] * sin(dir) + formula * vh * sin(stormDir);
      twind[i] = sqrt(pow(twindX, 2) + pow(twindY, 2));
      direction[i] = atan2(twindY, twindX) * 180 / pi;
      if (direction[i] < 0) {
        direction[i] += 360;
      }
      direction[i] = std::round(direction[i] * 1000.0) / 1000.0;
      twind[i] = std::round(twind[i] * 1000.0) / 1000.0;
    }
    return Rcpp::List::create(twind,
                        direction);
  }

// [[Rcpp::export]]
Rcpp::NumericVector
  computeDirectionBoose_cpp(const Rcpp::NumericVector x,
                            const Rcpp::NumericVector y,
                            const double lat,
                            const Rcpp::NumericVector landIntersect){
    const double pi = 3.141592653589793238463;
    const int s = x.size();
    double azimuth;
    Rcpp::NumericVector direction(s);
    double d_ = 0;
    double l_;

    for(int i=0; i<s; ++i){
      azimuth = -(atan2(y[i], x[i]) - pi / 2);
      l_ = landIntersect[i];
      if(azimuth < 0){
        azimuth += 2 * pi;
      }
      if(lat >= 0){
        d_ = azimuth * 180 / pi - 90;
        if(l_ == 1){
          d_ -= 40;
        }else{
          d_ -= 20;
        }
      }else{
        d_ = azimuth * 180 / pi + 90;
        if(l_ == 1){
          d_ += 40;
        }else{
          d_ += 20;
        }
      }
      if(d_ <0){
        d_ += 360;
      }
      if(d_ > 360){
        d_ -= 360;
      }
      direction[i] = std::round(d_ * 1000.0) / 1000.0;
      }
    return direction;
  }

// [[Rcpp::export]]
Rcpp::NumericVector
  computeDirection_cpp(const Rcpp::NumericVector x,
                       const Rcpp::NumericVector y,
                       const double lat){
    const double pi = 3.141592653589793238463;
    const int s = x.size();
    double azimuth;
    Rcpp::NumericVector direction(s);

    for(int i=0; i<s; ++i){
      azimuth = -(atan2(y[i], x[i]) - pi / 2);
      if(azimuth < 0){
        azimuth += 2 * pi;
      }
      if(lat >= 0){
        direction[i] = azimuth * 180 / pi - 90;
      }else{
        direction[i] = azimuth * 180 / pi + 90;
      }
      if(direction[i] <0){
        direction[i] = direction[i] + 360;
      }else{
        if(direction[i] > 360){
          direction[i] = direction[i] - 360;
        }
      }
    }
    return direction;
  }

// [[Rcpp::export]]
Rcpp::NumericVector
  computePDI_cpp(const Rcpp::NumericVector wind,
                 const double tempRes){
    double pdi = 0;
    const int s = wind.size();

    const double rho = 1.;
    const double cd = 0.002;
    for(int i=0; i<s; ++i){
      pdi += pdi * rho * cd;
    }
    return pdi * tempRes;
    }

// [[Rcpp::export]]
Rcpp::NumericVector
  computeExposure_cpp(const Rcpp::NumericVector wind,
                      const double tempRes,
                      const Rcpp::NumericVector threshold){
    const int s = wind.size();
    const int t = threshold.size();
    double tj;
    Rcpp::NumericVector exposure(t);
    for(int j=0; j<t; ++j){
      tj = threshold[j];
      for(int i=0; i<s; ++i){
        if(wind[i] >= tj){
          exposure[j] += tempRes;
        }
      }
    }
    return exposure;
  }