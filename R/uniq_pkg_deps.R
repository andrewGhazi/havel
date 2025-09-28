#' Count unique dependencies
#' @description A function for package developers to count which direct
#'   dependencies add the most unique dependencies (direct plus downstream) in
#'   total.
#'
#' @param pkg a package to check
#' @param order Consider combinations of this many packages. Be careful going
#'   beyond 3.
#' @details Use this function to identify which packages have the most unique
#'   downstream dependencies. For a package author, these are good targets to
#'   prioritize for removal.
#'
#'   Because of the graph structure of dependencies, sometimes there isn't any
#'   one package that adds a lot of unique dependencies, but there is a PAIR or
#'   TRIPLET do. Set the \code{order} argument to check
#'   which pairs, triplets, etc have the most unique dependencies. Be careful
#'   going beyond 3 because the number of combinations grows quickly, and I
#'   haven't optimized the internals of this function much :)
#' @returns A data.table listing the packages and the number of unique
#'   dependencies.
#' @seealso [plot_deps_graph()]
#' @examples
#' # pkgcache isn't allowed in examples, uncomment and run interactively:
#' # uniq_pkg_deps("ggplot2")
#' # ^ scales adds the most unique dependencies to ggplot2 -- 6 including itself.
#' @export
uniq_pkg_deps = function(pkg,
                         dep_type = c("depends", "imports", "linkingto"),
                         pak_res = NULL,
                         order = 1) {

  prgc = get_pkg_graph(pkg, dep_type, pak_res)

  pak_res = prgc[[1]]

  edge_vec = prgc[[2]]

  pkg = clean_pkg_nm(pkg, pak_res)

  evt = t(edge_vec)

  ns_df = get_deps_memo(evt[, 1], evt[, 2], pkg) |> list2DF()

  # as.data.table(ns_df)[,unlist(ds_deps), by = pkg]

  dir_deps = pak_res |> # TODO: move this behavior to separate function
    sbt(package %==% pkg) |>
    pull1("deps") |>
    sbt(tolower(type) %in% dep_type) |>
    sbt(ref != "R") |>
    get_elem("package")

  combn_df = combn(dir_deps, order) |>
    t() |>
    qDT() |>
    setColnames(paste0("p", 1:order)) |>
    mtt(group = 1:choose(length(dir_deps), order)) |>
    pivot(ids = "group", names = list("p_i", "pkg")) |>
    join(ns_df, on = "pkg", verbose = FALSE) |>
    roworder(group) |> # get_n_ds_uniq.cpp requires this ordering, don't drop it.
    qDT()

  # TODO: loop through groups

  ds_by_grp = combn_df |>
    slt(-p_i) |>
    data.table::as.data.table()

  tall_ds_by_grp = ds_by_grp[,.(pg = paste0(pkg, collapse=";"),
                                pl = funique(unlist(ds_deps))),
                             by = group]
  # ^ fast data.table unnesting

  # tall_ds_by_grp$pl[whichNA(tall_ds_by_grp$pl)] = NULL

  # setkey(tall_ds_by_grp, group) # sbt usually faster

  grp_memb_df = ds_by_grp |>
    slt(group, pkg) |>
    funique()

  # There's probably some fancy database-like operation for this, but we'll just
  # settle for the naive way: loop over groups. For each group, find any
  # dependencies unique to that group that are not found in the other groups.
  # You have to exclude other groups that include any of group i's packages.

  i_uniq = lapply(seq_len(max(combn_df$group)),
                  get_uniq_i,
                  tall_ds_by_grp,
                  grp_memb_df,
                  dir_deps)

  combn_df |>
    slt(group, p_i, pkg) |>
    pivot(how = "wider", names = "p_i", values = "pkg") |>
    roworder(group) |>
    mtt(n_uniq = lengths(i_uniq) + order,
        uniq_pkgs = i_uniq) |>
    roworder(-n_uniq) |>
    slt(-group)

}

get_uniq_i = function(i,
                      tall_ds_by_grp,
                      grp_memb_df,
                      dir_deps) {

  i_df = tall_ds_by_grp |> sbt(group %==% i)

  i_pkgs = grp_memb_df |>
    sbt(group %==% i) |>
    get_elem("pkg")

  any_i_df = grp_memb_df |>
    gby(group) |>
    mtt(any_i = any(pkg %in% i_pkgs)) |>
    fungroup() |>
    sbt(!any_i)

  anti_i = any_i_df$group |> funique()

  ni_df = tall_ds_by_grp |> sbt(group %iin% anti_i)

  ni_and_dir = funique(c(ni_df$pl, dir_deps))

  i_ds = na_rm(i_df$pl)

  i_ds[i_ds %!iin% ni_and_dir]

}
