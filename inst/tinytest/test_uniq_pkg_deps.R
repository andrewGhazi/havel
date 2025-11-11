test_uniq_pkg_deps = function() {
  res = havel::uniq_pkg_deps("ggplot2",
                             pak_res = havel::pkg_deps_ex$ggplot2)

  expect_equal(res$p1[1], "scales")

  expect_equal(res$n_uniq[1], 6)
}


test_uniq_pkg_deps_o2 = function() {
  res = havel::uniq_pkg_deps("ggplot2",
                             pak_res = havel::pkg_deps_ex$ggplot2,
                             order = 2)

  expect_equal(res$p1[1], "scales")

  expect_equal(res$p2[1], "withr")
  expect_equal(res$n_uniq[1], 8)
}

test_uniq_pkg_deps()
test_uniq_pkg_deps_o2()
