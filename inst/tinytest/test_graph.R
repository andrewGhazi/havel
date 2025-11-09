
#' TODO:
#' check larger example
#' check Bioc example
#' check local package in tempdir()
#' figure out if calling pak::pkg_deps is allowed on CRAN

test_graph = function() {
  og = getOption("repos")
  options(repos = "https://cloud.r-project.org")

  dt_res = havel:::get_pkg_graph("data.table",
                                 dep_type = c("depends", "imports", "linkingto"),
                                 pak_res = NULL,
                                 info_method = "pak")
  options("repos") = og

  dt_edges = dt_res[[2]]

  expect_equal(dt_edges,
               structure(c("data.table", "methods"),
                         dim = 2:1,
                         dimnames = list(
                           NULL, "data.table")))


}

test_graph_ggplot2 = function() {
  options(repos = "https://cloud.r-project.org")

  gg_res_t = havel:::get_pkg_graph("ggplot2",
                                 dep_type = c("depends", "imports", "linkingto"),
                                 pak_res = NULL,
                                 info_method = "pak")

  expect_identical(gg_res_t[[1]] |> slt(ref, direct, package, deps),
                   havel:::pak_results[[2]] |> slt(ref, direct, package, deps))


}

test_nonexistent = function() {
  expect_error(havel:::get_pkg_graph("idontexist",
                                     dep_type = c("depends", "imports", "linkingto"),
                                     pak_res = NULL,
                                     info_method = "pak"))
}

if (at_home()) {
  test_graph()
  test_graph_ggplot2()
  test_nonexistent()

}
