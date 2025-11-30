
test_small_plot = function() {
  # plot with 1 edge like data.table -> methods

  expect_silent(havel::plot_deps_graph('data.table@1.17.8',
                                       pak_res = havel::pkg_deps_ex$data.table))

}

test_empty_plot = function() {
  # plot with 0 edges like utils
  # This one will warn because pak::pkg_deps fails for utils
  og = getOption("repos")
  options(repos = "https://cloud.r-project.org")
  expect_message(havel::plot_deps_graph('utils'))
  if (og != "@CRAN@") options("repos" = og)

}

test_nontrivial_plot = function() {

  expect_silent(havel::plot_deps_graph('ggplot2@4.0.1',
                                       pak_res = havel::pkg_deps_ex$ggplot2 ))

}

test_small_plot()
if (at_home()) test_empty_plot()
test_nontrivial_plot()
