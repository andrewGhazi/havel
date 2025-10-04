
test_small_plot = function() {
  # plot with 1 edge like data.table -> methods

  expect_silent(havel::plot_deps_graph('data.table', font_family = "sans"))

}

test_empty_plot = function() {
  # plot with 0 edges like utils
  expect_silent(havel::plot_deps_graph('utils', font_family = "sans"))

}

test_nontrivial_plot = function() {

  expect_silent(havel::plot_deps_graph('ggplot2', font_family = "sans"))

}

test_nontrivial_plot()
