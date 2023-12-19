#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;
using namespace std;



// =============================================================================
// =====================ccp functions for models ===============================
// =============================================================================

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


// [[Rcpp::export]]
NumericVector
  holland_cpp(NumericVector r,
              double rmw,
              double msw,
              double pc,
              double poci,
              double lat){ //warning here

    double rho = 1.15; //air density
    double f = 2 * 7.29 * pow(10,-5) * sin(lat); //Coriolis parameter
    double b = rho * exp(1) * pow(msw, 2) / (poci - pc);

    int s = r.size();
    NumericVector vr(s);

    for(int i = 0; i < s; ++i) {
      vr[i] = sqrt(b / rho * pow(rmw / r[i], b) * (poci - pc) * exp(-pow(rmw / r[i], b)) + pow(r[i] * f / 2, 2)) - r[i] * f / 2;
      vr[i] = std::round(vr[i]* 1000.0) / 1000.0;
    }

    return vr;

  }


// [[Rcpp::export]]
NumericVector
  boose_cpp(NumericVector r,
            double rmw,
            double msw,
            double pc,
            double poci,
            double x,
            double y,
            double vx,
            double vy,
            double vh,
            double landIntersect,
            double lat){


    double rho = 1; //air density
    double b = rho * exp(1) * pow(msw, 2) / (poci - pc);

    int s = r.size();
    NumericVector vr(s);
    NumericVector angle(s);

    if(lat>= 0){
      // Northern Hemisphere, t is clockwise
      angle = atan2(vy, vx) - atan2(y, x);
    }else{
      // Southern Hemisphere, t is counterclockwise
      angle = atan2(y, x) - atan2(vy, vx);
    }

    for(int i = 0; i < s; ++i) {
      double v = sqrt(pow(rmw / r[i], b) * exp(1 - pow(rmw / r[i], b)));

      if(landIntersect == 1){
        vr[i] = 0.8 * (msw - (1 - sin(angle[i])) * vh / 2) * v;
      }else{
        vr[i] = (msw - (1 - sin(angle[i])) * vh / 2) * v;
      }

      vr[i] = std::round(vr[i]* 1000.0) / 1000.0;
    }

    return vr;
  }

// =============================================================================
// =====================ccp functions for asymmeties and directions ============
// =============================================================================

// // [[Rcpp::export]]
// List
//   computeAsymmetry_cpp(
//     String asymmetry,
//     NumericVector wind,
//     NumericVector x,
//     NumericVector y,
//     double vx,
//     double vy,
//     double vh,
//     double r,
//     double rmw,
//     double lat) {
// 
//     double pi = 3.141592653589793238463;
//     int s = wind.size();
//     NumericVector dir = -(atan(y / x) - pi / 2);
//     if (lat >= 0) {
//       dir = dir - pi / 2;
//     } else {
//       dir = dir + pi / 2;
//     }
// 
//     for(int i = 0; i < s; i++) {
//       if (dir[i] < 0) {
//         dir[i] += 2 * pi;
//       } else if (dir[i] > 2 * pi) {
//         dir[i] -= 2 * pi;
//       }
//     }
// 
//     NumericVector windX = wind * cos(dir);
//     NumericVector windY = wind * sin(dir);
// 
//     double stormDir = -(atan(vy / vx) - pi / 2);
// 
//     if (stormDir < 0){
//       stormDir += 2 * pi;
//     }
// 
//     double mWindX = vh * cos(stormDir);
//     double mWindY = vh * sin(stormDir);
// 
//     double formula = 0;
//     if (asymmetry == String::"Chen") { // ERROR: expected unqualified-id
//       formula = 3 * pow(rmw, 3 / 2) * pow(r, 3 / 2) / (pow(rmw, 3) + pow(r, 3) + pow(rmw, 3 / 2) * pow(r, 3 / 2));
//     } else if (asymmetry == String::"Miyazaki") { // ERROR: expected unqualified-id
//       formula = exp(-r / 500 * pi);
//     }
// 
//     NumericVector tWindX = windX + formula * mWindX;
//     NumericVector tWindY = windY + formula * mWindY;
// 
//     NumericVector twind = sqrt(pow(tWindX, 2) + pow(tWindY, 2));
//     NumericVector direction = atan(tWindY / tWindX) * 180 / pi;
// 
//     for(int i = 0; i < s; i++) {
//       if (direction[i] < 0) {
//         direction[i] += 360;
//       }
//     }
// 
//     return List::create(std::round(twind * 1000.0) / 1000.0, std::round(direction * 1000.0) / 1000.0); // ERROR: no matching function for call to 'round'
//   }

// // [[Rcpp::export]]
// NumericVector
//   computeDirectionBoose_cpp(NumericVector x,
//                             NumericVector y,
//                             double lat,
//                             NumericVector landIntersect){
//     
//     double pi = 3.141592653589793238463;
//     int s = x.size();
//     
//     NumericVector azimuth = -(atan2(y, x) - pi / 2); // ERROR: no matching function for call to 'atan2'
//     
//     for(int i=0; i<s; ++i){
//       if(azimuth[i] < 0){
//         azimuth[i] = azimuth[i] + 2 * pi;
//       }
//     }
//     
//     NumericVector direction(s);
//     
//     if(lat >= 0){
//       for(int i=0; i<s; ++i){
//         if(landIntersect[i] == 1){
//           direction = azimuth[i] * 180 / pi - 130;
//         }else{
//           direction = azimuth[i] * 180 / pi - 110;
//         }
//       }
//     }else{
//       for(int i=0; i<s; ++i){
//         if(landIntersect[i] == 1){
//           direction = azimuth[i] * 180 / pi - 50;
//         }else{
//           direction = azimuth[i] * 180 / pi - 70;
//         }
//       }
//     }
//     
//     for(int i=0; i<s; ++i){
//       if(direction[i] <0){
//         direction[i] = direction[i] + 360;
//       }else{
//         if(direction[i] > 360){
//           direction[i] = direction[i] - 360;
//         }
//       }
//     }
//     
//     return direction;
//     
//   }

// // [[Rcpp::export]]
// NumericVector
//   computeDirection_cpp(NumericVector x,
//                        NumericVector y,
//                        double lat){
//     
//     double pi = 3.141592653589793238463;
//     int s = x.size();
//     
//     NumericVector azimuth = -(atan2(y, x) - pi / 2); // ERROR: no matching function for call to 'atan2'
//     NumericVector direction(s);
//     
//     if(lat >= 0){
//       direction = azimuth * 180 / pi - 90;
//     }else{
//       direction = azimuth * 180 / pi + 90;
//     }
//     
//     for(int i=0; i<s; ++i){
//       if(direction[i] <0){
//         direction[i] = direction[i] + 360;
//       }else{
//         if(direction[i] > 360){
//           direction[i] = direction[i] - 360;
//         }
//       }
//     }
//     
//     return direction;
//     
//   }


