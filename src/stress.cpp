// The code in this file is licensed under the MIT license below (unlike the
// remainder of the 'havel' package, which is GPL-3).
//
// # MIT License
//
// Copyright (c) 2023 graphlayouts authors
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.


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
