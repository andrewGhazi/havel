
#' TODO:
#' check larger example
#' check Bioc example
#' check local package in tempdir()
#' figure out if calling pak::pkg_deps is allowed on CRAN

test_graph = function() {
  dt_res = havel:::get_pkg_graph("data.table",
                                     dep_type = c("depends", "imports", "linkingto"),
                                     pak_res = NULL)

  dt_edges = dt_res[[2]]

  expect_equal(dt_edges,
               structure(c("data.table", "methods"),
                         dim = 2:1,
                         dimnames = list(
                           NULL, "data.table")))


}
