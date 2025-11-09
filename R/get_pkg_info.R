
get_dep_of_type = function(pkg, type, db, base_db) {

  # Oops I enforced lowercase too early :)
  type_map = c("depends" = "Depends",
               "imports" = "Imports",
               "linkingto" = "LinkingTo",
               "suggests" = "Suggests")

  if (pkg %in% rownames(base_db)) {

    dep_vec = tools::package_dependencies(pkg,
                                          db = base_db,
                                          which = type_map[type])[[1]]

  } else {
    dep_vec = tools::package_dependencies(pkg,
                                          db = db,
                                          which = type_map[type])[[1]]
  }

  data.table(ref = pkg,
             type = type,
             package = dep_vec)

}

get_info_row = function(pkg, dep_type, db, base_db) {

  dir_deps = lapply(dep_type,
                    get_dep_of_type,
                    pkg = pkg,
                    db = db,
                    base_db = base_db)

  names(dir_deps) = dep_type

  dir_deps |> rbindlist()
}

#' @importFrom tools package_dependencies
#' @importFrom utils available.packages installed.packages
get_info_tools = function(pkg, dep_type) {
  # Return a data frame of info (columns: ref, direct, package, deps)
  # deps should be a list column of dfs with columns: ref, type, package

  # goal = pak::pkg_deps("ggplot2") |> slt(ref, direct, package, deps)

  db = utils::available.packages() # Want to avoid doing this many times

  base_db = utils::installed.packages(priority = "base")

  top_df = get_info_row(pkg, dep_type, db, base_db)

  to_get = top_df$package

  res = data.table(ref = pkg,
                   direct = TRUE,
                   package = pkg,
                   deps = list(top_df))

  checked = pkg

  while (!rlang::is_empty(to_get)) {
    cur = to_get[1]

    checked = funique(c(checked, cur))

    pkg_i = get_info_row(cur, dep_type, db, base_db)

    pkg_i_row = data.table(ref = cur,
                           direct = FALSE,
                           package = cur,
                           deps = list(pkg_i))

    res = rowbind(res, pkg_i_row) # Growing here might be slow, TODO: Cpp version?

    to_add = pkg_i$package[!(pkg_i$package %in% checked)]

    to_get = to_get[-1]

    to_get = funique(c(to_get, to_add))

  }

  # TODO: get this to handle local packages "."?

  res
}

#' @importFrom methods is
get_pkg_info = function(pkg, dep_type, info_method) {

  if (info_method == "tools") {
    pr = try(get_info_tools(pkg, dep_type),
             silent = TRUE)
  } else {
    pr = try(pak::pkg_deps(pkg),
             silent = TRUE)

    if (is(pr, "try-error")) {
      # can't use tryCatch() because the argument to error function doesn't provide the package name

      cli::cli_alert_info("{.fn pak::pkg_deps} failed to obtain package dependency information, falling back to {.fn tools::package_dependencies}")
      pr = try(get_info_tools(pkg, dep_type),
               silent = TRUE)
      # TODO: ^ find out why this DOESN'T return NULL like it should for pkg = "gsdfgsdfgas"
    }
  }

  if (is(pr, "try-error") | is.null(pr[[1]])) {
    cli::cli_abort("Failed to get package information with both {.fn pak::pkg_deps} and {.fn tools::package_dependencies}")
  }

  pr
}

get_pkg_graph = function(pkg, dep_type, pak_res, info_method) {

  # dep_type = c("depends", "imports", "linkingto")

  pak_res = pak_res %||% get_pkg_info(pkg, dep_type, info_method)

  # This should handle pkg = "." I think?
  pak_ss = ss(pak_res, pak_res$direct)
  if (pkg != pak_ss$ref[1]) pkg = pak_ss$package[1]

  # TODO: attach connection type as well

  # V This prints an empty message...
  nested_pkg_list = pak_res |>
    slt(c("package", "deps")) |>
    frename(from = "package")

  l = nested_pkg_list$deps

  names(l) = nested_pkg_list$from

  unnested = rowbind(l,
                     idcol = "from") |>
    frename(to = "package") |>
    qDT() |>
    ftransformv(vars = "from", FUN = as.character)

  edge_list = unnested |>
    sbt(tolower(unnested$type) %iin% dep_type) |>
    slt(c("from", "ref", "type", "to")) |>
    funique()

  edge_list = edge_list |>
    sbt(edge_list$to != "R")

  edge_vec = mapply(\(x,y) c(x,y),
                    edge_list$from, edge_list$to) |>
    unlist()

  return(list(pak_res, edge_vec))
}
