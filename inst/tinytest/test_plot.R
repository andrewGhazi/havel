
test_small_plot = function() {
  # plot with 1 edge like data.table -> methods

  expect_silent(havel::plot_deps_graph('data.table', font_family = "sans"))

}

test_empty_plot = function() {
  # plot with 0 edges like utils
  # This one will warn because pak::pkg_deps fails for utils
  expect_message(havel::plot_deps_graph('utils', font_family = "sans"))

}

test_nontrivial_plot = function() {

  expect_silent(havel::plot_deps_graph('ggplot2', font_family = "sans"))

}

if (at_home()) test_small_plot()
if (at_home()) test_empty_plot()
if (at_home()) test_nontrivial_plot()
