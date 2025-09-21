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
    roworder(group) |>
    qDT()

  # TODO: loop through groups

  # There's probably some fancy database-like operation for this, but we'll just
  # settle for the naive way: loop over groups. For each group, find any
  # dependencies unique to that group that are not found in the other groups.
  n_group = max(combn_df$group)

  empty_list = replicate(n_group, character(), simplify = FALSE)

  res_df = data.frame(group = seq_len(n_group)) |>
    mtt(group_pkgs = empty_list,
        n_uniq = 0,
        uniq_deps = empty_list)

  for (i in res_df$group) {
    # TODO: progress bar? This could be slow for high order number.
    # TODO: redo in C++

    i_df  = combn_df |> sbt(group %==% i)

    i_pkgs = i_df$pkg

    ni_df = combn_df |>
      gby(group) |>
      mtt(any_i = any(pkg %in% i_pkgs)) |>
      fungroup() |>
      sbt(!any_i)

    i_ds  =  i_df$ds_deps |> unlist() |> funique()
    i_ds = i_ds[i_ds %!iin% dir_deps]

    ni_ds = ni_df$ds_deps |> unlist() |> funique()

    res_i = i_ds[i_ds %!iin% ni_ds]

    res_df$group_pkgs[[i]] = i_df$pkg
    res_df$uniq_deps[[i]] = res_i
    res_df$n_uniq[i] = length(res_i) + order
  }

  res_df = res_df |>
    roworder(-n_uniq) |>
    slt(-group)

  id_df = res_df$group_pkgs |>
    unlist2d(idcols = FALSE) |>
    frename(\(x) gsub("V", "pkg", x))

  add_vars(id_df, res_df |> slt(-group_pkgs))


}
