
get_dep_of_type = function(pkg, type, db) {

  # Oops I enforced lowercase too early :)
  type_map = c("depends" = "Depends",
               "imports" = "Imports",
               "linkingto" = "LinkingTo",
               "suggests" = "Suggests")

  dep_vec = tools::package_dependencies(pkg,
                                        db = db,
                                        which = type_map[type])[[1]]

  data.table(ref = pkg,
             type = type,
             package = dep_vec)

}

get_info_row = function(pkg, dep_type, db) {

  dir_deps = lapply(dep_type,
                    get_dep_of_type,
                    pkg = pkg,
                    db = db)

  names(dir_deps) = dep_type

  dir_deps |> rbindlist()
}

#' @importFrom tools package_dependencies
#' @importFrom utils available.packages
get_info_tools = function(pkg, dep_type) {
  # Return a data frame of info (columns: ref, direct, package, deps)
  # deps should be a list column of dfs with columns: ref, type, package

  # goal = pak::pkg_deps("ggplot2") |> slt(ref, direct, package, deps)

  db = utils::available.packages() # Want to avoid doing this many times

  top_df = get_info_row(pkg, dep_type, db)

  dont_get = installed.packages(priority = "base")[,"Package"] # base packages that p_d can't get for some reason?

  to_get = top_df$package[!(top_df$package %in% dont_get)]

  res = data.table(ref = pkg,
                   direct = TRUE,
                   package = pkg,
                   deps = list(top_df))

  checked = pkg

  while (!rlang::is_empty(to_get)) {
    cur = to_get[1]

    checked = funique(c(checked, cur))

    pkg_i = get_info_row(cur, dep_type, db)

    pkg_i_row = data.table(ref = cur,
                           direct = FALSE,
                           package = cur,
                           deps = list(pkg_i))

    res = rowbind(res, pkg_i_row) # Growing here might be slow, TODO: Cpp version?

    to_add = pkg_i$package[!(pkg_i$package %in% c(checked, dont_get))]

    to_get = to_get[-1]

    to_get = funique(c(to_get, to_add))

  }

  # TODO: get this to handle local packages "."?

  res
}

get_pkg_info = function(pkg, dep_type) {

  pr = try(pak::pkg_deps(pkg),
           silent = TRUE)

  if (is(pr, "try-error")) {
    # can't use tryCatch() because the argument to error function doesn't provide the package name

    cli::cli_alert_info("{.fn pak::pkg_deps} failed to obtain package dependency information, falling back to {.fn tools::package_depencies}")
    pr = try(get_info_tools(pkg, dep_type),
             silent = TRUE)
    # TODO: ^ find out why this DOESN'T return NULL like it should for pkg = "gsdfgsdfgas"
  }

  if (is.null(pr[[1]])) {
    cli::cli_abort("Failed to get package information with both {.fn pak::pkg_deps} and {.fn tools::package_dependencies}")
  }

  pr
}

get_pkg_graph = function(pkg, dep_type, pak_res) {

  # dep_type = c("depends", "imports", "linkingto")

  pak_res = pak_res %||% get_pkg_info(pkg)

  # This should handle pkg = "." I think?
  if (pkg != sbt(pak_res, direct)$ref[1]) pkg = sbt(pak_res, direct)$package[1]

  # TODO: attach connection type as well

  # V This prints an empty message...
  nested_pkg_list = pak_res |>
    slt(package, deps) |>
    frename(from = package)

  l = nested_pkg_list$deps

  names(l) = nested_pkg_list$from

  unnested = rowbind(l,
                     idcol = "from") |>
    frename(to = "package") |>
    qDT() |>
    mtt(from = as.character(from))

  edge_list = unnested |>
    sbt(tolower(type) %in% dep_type) |>
    slt(from:to) |>
    funique() |>
    sbt(to != "R")

  edge_vec = mapply(\(x,y) c(x,y),
                    edge_list$from, edge_list$to) |>
    unlist()

  # TODO handle empty graphs

  return(list(pak_res, edge_vec))
}
