#' Count unique dependencies
#' @description A function for package developers to count which direct
#'   dependencies add the most unique dependencies (direct plus downstream) in
#'   total.
#'
#' @param pkg a package to check
#' @param order Consider combinations of this many packages. Be careful going
#'   beyond 3.
#' @details Use this function to tabulate which packages have the most unique
#'   downstream dependencies. For a package author, these are good targets to
#'   prioritize for removal if possible.
#'
#'   Because of the graph structure of dependencies, sometimes there isn't any
#'   one package that adds a lot of unique dependencies, but there is a PAIR or
#'   TRIPLET do. Set the \code{order} argument to check which pairs, triplets,
#'   etc have the most unique dependencies.
#' @returns A data.table listing the packages, number of unique dependencies,
#'   and the unique dependencies themselves.
#' @seealso \code{\link{plot_deps_graph}}
#' @inheritParams plot_deps_graph
#' @examples
#' uniq_pkg_deps("ggplot2", pak_res = pkg_deps_ex$ggplot2)
#' # ^ scales adds the most unique dependencies to ggplot2 -- 6 including itself.
#' # The `pak_res` argument here is a pre-computed result to avoid internet
#' # access while running the examples on CRAN's servers. It's not required.
#' @importFrom utils combn
#' @export
uniq_pkg_deps = function(pkg,
                         dep_type = c("depends", "imports", "linkingto"),
                         pak_res = NULL,
                         info_method = "pak",
                         order = 1) {

  prgc = get_pkg_graph(pkg, dep_type, pak_res, info_method = info_method)

  pak_res = prgc[[1]]

  edge_vec = prgc[[2]]

  pkg = clean_pkg_nm(pkg, pak_res)

  evt = t(edge_vec)

  ns_df = get_deps_memo(evt[, 1], evt[, 2], pkg) |> list2DF()

  top_level_res = pak_res |> # TODO: move this behavior to separate function
    sbt(pak_res$package %==% pkg) |>
    pull1("deps")

  dir_deps = top_level_res |>
    sbt(top_level_res$ref != "R" & tolower(top_level_res$type) %in% dep_type) |>
    get_elem("package")

  combn_df = get_combn_df(dir_deps, order, ns_df)

  ds_by_grp = combn_df |>
    data.table::as.data.table()

  slt(ds_by_grp, "p_i") = NULL

  tall_ds_by_grp = ds_by_grp[,list(pg = paste0(pkg, collapse=";"),
                                   pl = funique(unlist(.SD))),
                             by = group,
                             .SDcols = "ds_deps"]
  # ^ fast data.table unnesting

  grp_memb_df = ds_by_grp |>
    slt(group, pkg) |>
    funique() |>
    roworder("group")

  # There's probably some fancy database-like operation for this, but we'll just
  # settle for the naive way: loop over groups. For each group, find any
  # dependencies unique to that group that are not found in the other groups.
  # You have to exclude other groups that include any of group i's packages.

  i_uniq = lapply(seq_len(max(combn_df$group)),
                  get_uniq_i,
                  tall_ds_by_grp,
                  grp_memb_df,
                  dir_deps)

  res = combn_df |>
    slt("group", "p_i", "pkg") |>
    pivot(how = "wider", names = "p_i", values = "pkg") |>
    roworderv("group") |>
    mtt(n_uniq = lengths(i_uniq) + order,
        uniq_pkgs = i_uniq) |>
    roworderv("n_uniq", decreasing = TRUE)

  slt(res, "group") = NULL

  res

}

get_uniq_i = function(i,
                      tall_ds_by_grp,
                      grp_memb_df,
                      dir_deps) {

  i_df = tall_ds_by_grp |> sbt(tall_ds_by_grp$group %==% i)

  i_pkgs = grp_memb_df |>
    sbt(grp_memb_df$group %==% i) |>
    get_elem("pkg")

  any_memb_df = grp_memb_df |>
    add_vars(any_i = group_memb_check(grp_memb_df$group,
                                      grp_memb_df$pkg,
                                      i_pkgs))

  any_memb_df = any_memb_df |> ss(!any_memb_df$any_i)

  anti_i = funique(any_memb_df$group)

  ni_df = tall_ds_by_grp |> ss(tall_ds_by_grp$group %iin% anti_i)

  ni_and_dir = funique(c(ni_df$pl, dir_deps))

  i_ds = na_rm(i_df$pl)

  i_ds[i_ds %!iin% ni_and_dir]

}

get_combn_df = function(dir_deps, order, ns_df) {
  combn_df = combn(dir_deps, order) |>
    t() |>
    qDT() |>
    setColnames(paste0("p", 1:order)) |>
    mtt(group = 1:choose(length(dir_deps), order)) |>
    pivot(ids = "group", names = list("p_i", "pkg")) |>
    join(ns_df, on = "pkg", verbose = FALSE) |>
    roworder("group") |> # get_n_ds_uniq.cpp requires this ordering, don't drop it.
    qDT()

}
