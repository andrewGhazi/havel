#include <Rcpp.h>
#include <map>
using namespace Rcpp;

// [[Rcpp::export]]
double stress(NumericMatrix x, NumericMatrix D) {

  double stress = 0;
  int n = x.nrow();

  double d = 0;
  double xd = 0;
  double yd = 0;

  for (int i = 0; i < (n-1); ++i) {
    for (int j = (i+1); j < n; ++j) {
      xd = x(i,0) - x(j,0);
      yd = x(i,1) - x(j,1);

      d = sqrt(xd*xd + yd*yd) - D(i,j);

      stress += d*d;
    }
  }

  return(stress);
}

// [[Rcpp::export]]
NumericMatrix stress_layout(NumericMatrix y, NumericMatrix D, int iter, double tol) {
  int n = y.nrow();
  double n_inv = 1.0 / double(n);

  NumericMatrix x(clone(y));

  double stress_prev = stress(x, D);
  double stress_cur = 0.0;
  double eps = 0.0;
  double denom = 0.0;
  double xd = 0.0;
  double yd = 0.0;

  NumericMatrix x_cur(n,2);

  for (int k = 0; k < iter; ++k) {
    x_cur.fill(0.0);

    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
        if (i != j) {
          xd = x(i,0) - x(j,0);
          yd = x(i,1) - x(j,1);

          denom = sqrt(xd*xd + yd*yd);

          if (denom > 1e-5) {
            x_cur(i, 0) += x(j,0) + D(i,j) * xd / denom;
            x_cur(i, 1) += x(j,1) + D(i,j) * yd / denom;
          }
        }
      }
      x_cur(i,0) *= n_inv;
      x_cur(i,1) *= n_inv;
    }

    stress_cur = stress(x_cur, D);
    eps = (stress_prev - stress_cur) / stress_prev;

    if (eps <= tol) {
      // Rcout << "eps: " << eps << "\n";
      // Rcout << "k: " << k << "\n";
      break;
    }

    stress_prev = stress_cur;
    x = clone(x_cur);
  }

  return(x);
}
