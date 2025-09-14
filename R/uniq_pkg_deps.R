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

  ns_df = get_deps_memo(evt[, 1], evt[, 2], pkg)

  # TODO: adapt from here

  dir_deps = pak_res |>
    sbt(direct) |>
    pull1("deps") |>
    sbt(ref != "R" &
          !grepl("[Ss]uggests|[Ee]nhances", type) &
          !fduplicated(package))

  if (nrow(dir_deps) == 0) {
    cli::cli_alert_success("No dependencies")
    return(invisible(NULL))
  }

  all_deps = pak_res$deps |>
    rowbind() |>
    mtt(type = tolower(type)) |>
    sbt(type %in% c("depends", "imports", "linkingto") & package != "R") |>
    getElement("package") |>
    funique()

  pkg_combn = combn(dir_deps$package, order)

  n_dir = nrow(dir_deps) # number of direct dependencies
  n_tot = length(all_deps)
  n_com = ncol(pkg_combn)

  dep_mat = matrix(0,
                   ncol = n_dir + 1,
                   nrow = n_tot,
                   dimnames = list(all_deps,
                                   c(dir_deps$package, pkg)))


  for (i in seq_len(n_dir)) {
    # dep_deps = dep_deps |>
    #   pull1(deps) |>
    #   sbt(type %in% c("depends", "imports", "linkingto") & package != "R") |>
    #   get_elem("package")
    # ^ This doesn't work. pak doesn't list indirect dependencies in `package`.
    # Need to form the graph then look up children from there.

    all_ds = igraph::neighborhood(gr,
                                  order = n_tot,
                                  nodes = dir_deps$package[i],
                                  mode = "out",
                                  mindist = 1) |>
      lapply(names) |>
      unlist() |>
      funique()

    iv = all_deps %in% all_ds

    dep_mat[iv,i] = 1
  }
  dep_mat[,n_dir+1] = 1

  n_uniq = vector("numeric", n_com)
  udv = vector("list", n_com)

  for (i in seq_len(n_com)) {

    compl = dep_mat[,-which(dir_deps$package %in% pkg_combn[,i]),
                    drop=FALSE]

    od = rowSums(compl) == 1

    ud = od & !(names(od) %in% dir_deps$package)

    n_uniq[i] = sum(ud)
    udv[[i]] = names(which(ud))
  }

  pkg_combn |>
    t() |>
    qDT() |>
    setColnames(paste0("pkg", 1:order)) |>
    mtt(n_uniq_deps = n_uniq + order,
        uniq_deps = udv) |> # add order because things depend on themselves at least
    roworder(-n_uniq_deps)

}
