
get_gr_layout = function(df, nds, init) {

  gr = cppRouting::makegraph(df, directed = FALSE)

  D = cppRouting::get_distance_matrix(gr, from = nds, to = nds)

  if (init == "mds") {
    x_init = stats::cmdscale(D) + rnorm(2*nrow(D), sd = .3)
  } else {
    x_init = matrix(rnorm(2*nrow(D), sd = .3), ncol = 2)
  }

  stress_layout(x_init, D, iter = 10, tol = 1e-3)
}

get_igraph_gr = function(pkg, edge_vec) {

  if (length(edge_vec) == 0) {
    gr = igraph::make_empty_graph(1) |>
      igraph::set_vertex_attr("label", value = pkg) |>
      igraph::set_vertex_attr("name", value = pkg)
  } else {
    gr = igraph::make_directed_graph(edge_vec[,ncol(edge_vec):1])
  }

  gr
}


#' Plot dependency graph
#' @description Plot a package's dependency graph, coloring each node by the
#'   number of packages it depends on.
#' @param pkg package string passed to \code{\link[pak:pkg_deps]{pak::pkg_deps}}
#' @param dep_type type(s) of dependencies to look up. Valid values are
#'   \code{c("depends", "imports", "linkingto", "suggests")}
#' @param init how to initialize layout, MDS (default) or randomly
#' @param n_iter number of iterations for stress graph layout computation
#' @param pak_res a pre-computed result from
#'   \code{\link[pak:pkg_deps]{pak::pkg_deps}}
#' @param gg If true, use ggplot2 + ggraph to draw the plot instead of base
#'   graphics.
#' @param lwd line width
#' @param cex text size multiplication factor (see
#'   \code{\link[graphics:par]{graphics::par}})
#' @param pad_h height padding
#' @param pad_w width padding
#' @param arw factor by which to lengthen/shorten arrowheads
#' @param log_col_scale if TRUE, set node colors to vary on a log scale with
#' @details If you include \code{"suggests"} among the dependency types to look
#'   up, be aware that suggests can be circular / cyclic. If this is detected,
#'   the node coloring will be turned off.
#'
#'   Pre-computing the dependency lookup with
#'   \code{\link[pak:pkg_deps]{pak::pkg_dep}} and passing it to the
#'   \code{pak_res} argument can be handy when fiddling with graphical
#'   parameters. Also handy to avoid hitting the bundled GitHub PAT limits used
#'   by pak::pkg_deps().
#'
#'   The arrows will be off if you resize the panel with the base graphics
#'   version. Either set \code{gg=TRUE} or set your desired graphics device
#'   size, then re-run your command.
#'
#'   The layout in the base graphics version is stochastic - if it looks weird
#'   just run it again.
#'
#'   Random layout initialization only applies to base graphics, and typically
#'   looks worse than MDS initialization. Generally only useful if the MDS
#'   layout happens to overlap the legend or something.
#'
#' @returns a ggplot
#' @import collapse
#' @importFrom stats rnorm cmdscale
#' @examples
#' # Using pkgcache in examples is not allowed, uncomment to run these interactively:
#' # plot_deps_graph("ggplot2") # ggplot2 has many downstream dependencies
#' # plot_deps_graph("rlang") # rlang has only one
#'
#' @export
plot_deps_graph = function(pkg,
                           dep_type = c("depends", "imports", "linkingto"),
                           pak_res = NULL,
                           n_iter = 100,
                           init = "mds",
                           gg = FALSE,
                           lwd = 1,
                           cex = 1,
                           pad_h = .09,
                           pad_w = .07,
                           arw = 1,
                           log_col_scale = FALSE) {

  if (gg) {
    rlang::check_installed(c("ggplot2", "ggraph", "igraph", "pals"))
  }

  if (log_col_scale) {
    cli::cli_abort("log_col_scale not implemented yet!")
  }

  if ("suggests" %in% tolower(dep_type)) {
    cli::cli_abort("Suggests aren't properly handled yet. (fun fact: Suggests can be cyclical!)")
  }

  rlang::arg_match(dep_type,
                   values = c("depends", "imports", "suggests", "linkingto"),
                   multiple = TRUE)

  prgc = get_pkg_graph(pkg, dep_type, pak_res)

  pak_res = prgc[[1]]

  edge_vec = prgc[[2]]

  pkg = clean_pkg_nm(pkg, pak_res)

  evt = t(edge_vec)

  ns_df = get_deps_memo(evt[, 1], evt[, 2], pkg)

  df = data.frame(from = edge_vec[1,],
                  to = edge_vec[2,],
                  cost = 1)

  nds = funique(c(df$from, df$to))

  n = length(nds)

  if (gg) {
    gr = get_igraph_gr(pkg, edge_vec)

    gg_pkg_graph(pkg, gr, dep_type,
                 lwd, pad_h, pad_w, cex)
  } else {

    layout_mat = get_gr_layout(df, nds, init)

    # layout_mat = stress_layout(x_init, D, n_iter, 1e-3) # TODO make configurable

    plot_df = layout_mat |>
      qDT() |>
      mtt(pkg = nds) |>
      join(ns_df, on = "pkg", verbose = FALSE) |>
      mtt(n_deps = lengths(ds_deps),
          col_pos = floor(99*n_deps / max(n_deps) + 1),
          pkg_col = parula[col_pos])

    draw_pkg_graph(plot_df, evt, pkg,
                   lwd = lwd,
                   pad_h = pad_h,
                   pad_w = pad_w,
                   cex = cex)

  }
}

gg_pkg_graph = function(pkg, gr, dep_type,
                        lwd, pad_h, pad_w, cex) {

  # TODO call this fn, pass in args
  # TODO add adaptive label color to this version
  # TODO stretch goal: widen x range to allow for long pkg names on edges

  ec = igraph::ecount(gr)

  if (ec > 0 ) {
    edges_geom = ggraph::geom_edge_link(arrow = grid::arrow(length = grid::unit(1.5, "mm"),
                                                            type = "closed"),
                                        ggplot2::aes(end_cap = ggraph::label_rect(node2.name),
                                                     lwd = .33*lwd),
                                        color = "#222222")
  } else {
    edges_geom = NULL
  }

  if ("suggests" %in% dep_type && !igraph::is_acyclic(gr)) {
    cli::cli_alert_warning("Cycle detected among suggested packages. Can't color nodes by number of dependencies.")
    fill_aes = ggplot2::aes(label = name)
    fill_scale = NULL
  } else {
    fill_scale = ggplot2::scale_fill_gradientn(colors = pals::parula(100)[12:97])
    fill_aes = if (ec == 0) {
      ggplot2::aes(label = pkg)
    } else {
      ggplot2::aes(label = name,
                   fill = igraph::neighborhood_size(gr,
                                                    mode = "out",
                                                    order = ec,
                                                    mindist = 1))
    }

  }

  if (pad_h != .09) cli::cli_alert_warning("{.var pad_h} ignored in ggplot version, {.var pad_w} used for padding all sides.")

  gr |>
    ggraph::ggraph(ifelse(ec > 1, "stress", "tree")) +
    edges_geom +
    ggraph::geom_node_label(fill_aes,
                            label.padding = grid::unit(3*pad_w, "lines"),
                            size = 3.88*cex) + # Default text size = 3.88
    fill_scale +
    ggplot2::theme_dark() +
    ggplot2::theme(axis.title        = ggplot2::element_blank(),
                   axis.text         = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   plot.background   = ggplot2::element_rect(fill = "#444444",
                                                             color = "#444444"),
                   panel.grid        = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = "#666666"),
                   title             = ggplot2::element_text(color = "grey94"),
                   legend.title      = ggplot2::element_text(color = "#111111"),
                   legend.text       = ggplot2::element_text(color = "#111111"),
                   legend.ticks      = ggplot2::element_line(color = "#111111")) +
    ggplot2::labs(fill = "n_deps",
                  title = pkg)

}

# get_region = function(th, p2_th, pm_th, pmp2_th) {
#
# }

#' @importFrom grDevices rgb
#' @importFrom graphics arrows par rect text title
draw_pkg_graph = function(plot_df, evt, pkg, lwd,
                          pad_h = pad_h,
                          pad_w = pad_w,
                          cex = cex) {

  lght = "grey95"

  # par ---------------------------------------------------------------------

  par(bg = "grey43",
      mar = c(2,2,1.5,1),
      cex = 1.2,
      family = "Arial",
      adj = 0) # TODO handle this appropriately? i.e. store op <- par() and do par(op) at the end?

  cxy = par("cxy")

  plot_df = plot_df |>
    mtt(ws = V1 - .5 * nchar(pkg) * cxy[1],
        we = V1 + .5 * nchar(pkg) * cxy[1],
        ts = V2 - .5 * cxy[2],
        te = V2 + .5 * cxy[2])

  xr = c(fmin(plot_df$ws) - 2*pad_w, fmax(plot_df$we) + 2*pad_w)

  yr = c(fmin(plot_df$ts) - 2*pad_h, fmax(plot_df$te) + 2*pad_h)


  # initialize plot ---------------------------------------------------------


  plot(
    x = NULL,
    y = NULL,
    pch = NULL,
    bg = "grey10",
    axes = FALSE,
    xlim = xr,
    ylim = yr,
    xlab = "",
    ylab = ""
  )

  # legend ------------------------------------------------------------------

  # TODO: draw the legend at the end so it's on top?

  li = floor(seq(1,100, length.out = 30))

  labs = seq(1, max(plot_df$n_deps), length.out = 4) |>
    floor() |>
    as.character()

  lcols = parula[li]

  ly = yr[1] + .75*diff(yr) + (1:30 / 30 * .2*diff(yr))

  lye = ly + (1 / 30 * .2*diff(yr))

  lx = xr[1] + .95*diff(xr)

  lxe = lx + .02*diff(xr)

  rect(lx, ly, lxe, lye, col = lcols, border = rgb(0,0,0,0))

  text(cex = .85,
       labels = "# deps",
       x = lx,
       y = max(lye) ,
       pos = 3,
       offset = .3,
       col = lght)

  text(lx, ly[c(1,10,20,30)],
       labels = labs,
       pos = 2,
       offset = .2,
       col = lght,
       cex = .5)

  # title -------------------------------------------------------------------

  par(adj = 0)

  title(pkg, col.main = lght)

  par(adj = .5)

  # rects/labels/arrows data ------------------------------------------------

  plot_df = plot_df |>
    mtt(w = graphics::strwidth(pkg, cex = cex) + pad_w,
        h = graphics::strheight("T", cex = cex) + pad_h,
        xs = V1 - w/2,
        xe = V1 + w/2,
        ys = V2 - h/2,
        ye = V2 + h/2,
        text_col = c("#F2F2F2", "grey15")[(col_pos > 50) + 1]) |>
    roworder(n_deps) # top-level package will always be on top

  arrow_df = evt |>
    qDT() |>
    setColnames(c("p1", "p2")) |>
    join(plot_df |> slt(p1 = pkg, p1x = V1, p1y = V2), verbose = FALSE) |>
    join(plot_df |> slt(p2 = pkg, p2x = V1, p2y = V2, xs:ye), verbose = FALSE) |>
    mtt(th = atan2(p2y - p1y, p2x - p1x), #theta
        p2_th = atan2(p2y-ys, p2x-xs), # angle from the corner to the center of the box at pkg 2
        p2_high = as.integer(p2y > p1y),
        reg = get_region(abs(th), p2_th, p2_high, pi))  |>
    mtt(get_axy(reg, xs, xe, ys, ye, th, p2x, p2y)) # needs to return an n x 2 df with columns ax and ay

  # loop through dependencies -----------------------------------------------

  # You have to loop because labels won't overlap their respective rectangles
  # properly if you do all one then the other. Also it makes arrows cross
  # over/under in the most pleasing way.

  for (i in 1:nrow(plot_df)) {

    arrow_i = arrow_df |> sbt(whichv(p1, plot_df$pkg[i]))

    arrows(arrow_i$p1x,
           arrow_i$p1y,
           arrow_i$ax,
           arrow_i$ay,
           lwd = lwd,
           col = "grey14",
           length = .25*plot_df$h[1],
           angle = 20)

    #TODO: make border optionally red if it's a direct dependency of the top-level one.
    rect(
      col = plot_df$pkg_col[i],
      xleft = plot_df$xs[i],
      xright = plot_df$xe[i],
      ybottom = plot_df$ys[i],
      ytop = plot_df$ye[i],
      border = rgb(0,0,0,0)
    )

    text(
      x = plot_df$V1[i],
      y = plot_df$V2[i],
      cex = cex,
      col = plot_df$text_col[i],
      labels = plot_df$pkg[i]
    )
  }
}


get_pkg_graph = function(pkg, dep_type, pak_res) {

  dep_type = c("depends", "imports", "linkingto")

  pak_res = pak_res %||% pak::pkg_deps(pkg)
  # ^ TODO: try to fall back to tools::package_dependencies() if this fails

  # This should handle pkg = "." I think?
  if (pkg != sbt(pak_res, direct)$ref[1]) pkg = sbt(pak_res, direct)$package[1]

  # V This prints an empty message...
  nested_pkg_list = pak_res |>
    slt(package, deps) |>
    frename(from = package)

  names(nested_pkg_list$deps) = nested_pkg_list$from

  unnested = rowbind(nested_pkg_list$deps,
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

