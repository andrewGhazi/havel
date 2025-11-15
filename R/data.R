#' Example results from \code{\link[pak:pkg_deps]{pkg_deps}}
#'
#' This is a named list containing pre-computed examples of package dependency
#' information for packages \code{data.table} (v1.17.8), \code{ggplot2}
#' (v4.0.1), and \code{utils} pulled on Monday November 15th 2025. The first two
#' use \code{\link[pak:pkg_deps]{pkg_deps}} from the \code{pak} package and the
#' last one uses an internal function in \code{havel} that falls back to
#' \code{\link[tools:package_dependencies]{package_dependencies}} from
#' \code{tools} (\code{\link[pak:pkg_deps]{pkg_deps}} fails on base packages).
#' These precomputed results are used to provide data to run the examples
#' without requiring internet access (notably on CRAN's servers).
#' @format A named list containing three data frames of package dependency
#'   information:
#' \describe{
#'   \item{data.table}{a tibble describing \code{data.table}'s dependencies}
#'   \item{ggplot2}{a tibble describing \code{ggplot2}'s dependencies}
#'   \item{utils}{a data.table describing \code{utils}'s dependencies}
#' }
"pkg_deps_ex"
