
dt_res = pak::pkg_deps("data.table")
gg_res = pak::pkg_deps("ggplot2")
ut_res = havel:::get_info_tools("utils", dep_type = c("depends", "imports", "linkingto"))

pak_results = list(dt_res, gg_res, ut_res)

usethis::use_data(pak_results, overwrite = TRUE, internal = TRUE)
