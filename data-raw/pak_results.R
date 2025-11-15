
dt_res = pak::pkg_deps("data.table@1.17.8")
gg_res = pak::pkg_deps("ggplot2@4.0.1")
ut_res = havel:::get_info_tools("utils", dep_type = c("depends", "imports", "linkingto"))

pkg_deps_ex = list("data.table" = dt_res,
                   "ggplot2" = gg_res,
                   "utils" = ut_res)

usethis::use_data(pkg_deps_ex, overwrite = TRUE)
