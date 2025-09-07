
#include <Rcpp.h>
#include <map>
using namespace Rcpp;

std::map<String, CharacterVector> update_map(CharacterVector from, CharacterVector to, String pkg, std::map<String, CharacterVector> m) {
  if (m.count(pkg) > 0) {
    // The pkg is already in the map
    return(m);
  } else {
    int n = from.length();

    CharacterVector dd; // direct dependencies

    for (int i = 0; i < n; ++i) {
      if (from(i) == pkg) dd.push_back(to(i));
    }

    if (dd.length() > 0) {
      List indir(dd.length());
      CharacterVector ds; // downstream deps

      for (int i = 0; i < dd.length(); ++i) {
        // indir[i] = get_deps(from, to, dd(i));
        m = update_map(from, to, dd(i), m);
        indir[i] = m[dd(i)];
      }

      for (int i = 0; i < dd.length(); ++i) {
        CharacterVector ii = indir[i];

        // stick the i-th downstream deps onto ds
        for (int j = 0; j < ii.length(); ++j) {
          ds.push_back(ii(j));
        }
      }

      ds = unique(ds);

      // stick all the downstream dependencies onto dd, unique() it later
      for (int i = 0; i < ds.length(); ++i) {
        dd.push_back(ds(i));
      }
    }

    m[pkg] = unique(dd);
  }

  return(m);
}

// [[Rcpp::export]]
List get_deps_memo(CharacterVector from, CharacterVector to, String pkg) {
  int n = from.length();

  std::map<String, CharacterVector> m;

  m = update_map(from, to, pkg, m);

  CharacterVector pkgs;
  List ds_deps;

  for (const auto& n : m) {
    pkgs.push_back(n.first);
    ds_deps.push_back(n.second);
  }

  List res = List::create(Named("pkg") = pkgs,
                          Named("ds_deps") = ds_deps);

  return(res);
}

// [[Rcpp::export]]
IntegerVector get_region(NumericVector abs_th, NumericVector p2_th, IntegerVector p2_high, double pi) {
  int n = abs_th.length();

  IntegerVector res(n);
  double thi = 0.0;
  double p2i = 0.0;
  // double ath = 0.0;
  // int sp2 = 0;

  for (int i=0; i<n; ++i) {
    thi = abs_th[i];
    p2i = p2_th[i];

    // ath = abs(thi);
    // sp2 = sign_x(p2i);

    if (p2_high[i]) {
      if (thi < p2i) {
        res[i] = 4; continue; // left
      } else if (thi > (pi - p2i)) {
        res[i] = 2; continue; // right
      } else {
        res[i] = 1; continue; // below
      }
    } else {
      if (thi < p2i) {
        res[i] = 4; continue; // left
      } else if (thi > (pi - p2i)) {
        res[i] = 2; continue; // right
      } else {
        res[i] = 3; continue; // above
      }
    }
  }

  return(res);
}

// [[Rcpp::export]]
DataFrame get_axy(IntegerVector reg,
                  NumericVector xs,
                  NumericVector xe,
                  NumericVector ys,
                  NumericVector ye,
                  NumericVector th,
                  NumericVector p2x,
                  NumericVector p2y) {
  int n = reg.length();

  NumericVector ax(n);
  NumericVector ay(n);
  NumericVector tan_th(n);

  tan_th = tan(th);

  double dy;
  double dx;

  for (int i = 0; i < n; ++i ) {
    if (reg[i] == 1) {
      ay[i] = ys[i];

      dy = p2y[i] - ys[i];

      ax[i] = p2x[i] - dy / tan_th[i];

    } else if (reg[i] == 3) {
      ay[i] = ye[i];

      dy = p2y[i] - ys[i];

      ax[i] = p2x[i] + (dy / tan_th[i]);

    } else if (reg[i] == 2) {
      ax[i] = xe[i];

      dx = xe[i] - p2x[i];

      dy = dx * tan_th[i];

      ay[i] = p2y[i] + dy;
    } else if (reg[i] == 4) {
      ax[i] = xs[i];

      dx = xs[i] - p2x[i];

      dy = dx * tan_th[i];

      ay[i] = p2y[i] + dy;

    }

  }

  DataFrame res = DataFrame::create(Named("ax") = ax, Named("ay") = ay);
  return(res);
}
