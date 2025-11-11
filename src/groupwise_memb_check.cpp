#include <Rcpp.h>
using namespace Rcpp;

int my_in(String x, CharacterVector i_pkgs, int n) {

  int res = 0;

  // match().length();
  for (int i=0; i<n; ++i) {
    if (i_pkgs[i] == x) res +=1;
  }

  return(res);
}

// [[Rcpp::export]]
LogicalVector group_memb_check(IntegerVector group, CharacterVector pkg, CharacterVector i_pkgs) {

  int n = pkg.length();
  LogicalVector res (n);

  int i_strt = 0;
  int grp_cur = group[0];
  int cur_res = 0;
  int i_pkg_l = i_pkgs.length();

  for (int i = 0; i < n; ++i) {

    // Rcout << "grp_cur:" << grp_cur << "\n";
    // Rcout << "ic" << (group[i] == grp_cur) << "\n";
    // Check if we're still in the current group
    if (group[i] == grp_cur) {
      // Check if the ith package is in the set
      // if (i == 2) Rcout << "i_chk: " << my_in(pkg[i], i_pkgs, i_pkg_l) << "\n";
      if (my_in(pkg[i], i_pkgs, i_pkg_l) != 0) cur_res += 1;
      continue;
    } else {
      // We've reached a new group

      // Assign the results for the previous group
      for (int j = i_strt; j < (i); ++j) {
        res[j] = cur_res;
      }

      // Reset tracking quantities
      i_strt = i;
      grp_cur = group[i];
      cur_res = 0;

      // and check the ith package
      if (my_in(pkg[i], i_pkgs, i_pkg_l) != 0) cur_res += 1;
    }
  }

  // Assign the results for the last group
  for (int j = i_strt; j < n; ++j) {
    res[j] = cur_res;
  }

  return(res);
}
