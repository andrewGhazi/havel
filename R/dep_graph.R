

parula = c("#352A87", "#342D8F", "#343197", "#34359F", "#3439A7", "#343DAF",
           "#2F41B7", "#2946BE", "#234BC6", "#1D50CE", "#1755D5", "#125AD9",
           "#0F5EDB", "#0C61DC", "#0865DD", "#0569DF", "#036CDF", "#066FDE",
           "#0872DC", "#0B75DB", "#0D77DA", "#107AD8", "#107CD7", "#117FD6",
           "#1181D5", "#1284D4", "#1286D3", "#1189D2", "#0F8CD2", "#0D90D1",
           "#0B93D1", "#0996D1", "#0898CF", "#079BCE", "#079DCC", "#06A0CB",
           "#06A2C9", "#06A4C7", "#07A6C4", "#08A8C2", "#0AAABF", "#0BABBC",
           "#0DADB9", "#11AEB6", "#16B0B2", "#1AB1AF", "#1FB2AC", "#23B4A8",
           "#29B5A5", "#2FB6A1", "#36B89E", "#3CB99A", "#42BA97", "#49BB93",
           "#51BC8F", "#58BC8B", "#60BD88", "#67BD84", "#6EBE80", "#75BE7D",
           "#7DBE7A", "#84BE77", "#8BBE74", "#91BE71", "#97BE6F", "#9DBE6C",
           "#A3BD6A", "#A9BD67", "#AFBC65", "#B5BC63", "#BABC61", "#C0BB5F",
           "#C5BB5C", "#CBBA5A", "#D0BA58", "#D5BA56", "#DAB953", "#E0B951",
           "#E5B94F", "#E9B94B", "#EEBA48", "#F2BB44", "#F6BC40", "#FBBD3D",
           "#FCC03A", "#FCC336", "#FBC633", "#FBC930", "#FBCD2D", "#FAD02A",
           "#F9D428", "#F8D725", "#F7DB22", "#F5DF20", "#F5E31D", "#F5E71A",
           "#F6EC17", "#F7F114", "#F8F611", "#F9FB0E")

get_gr_layout = function(edge_vec) {

  df = data.frame(from = edge_vec[1,],
                  to = edge_vec[2,],
                  cost = 1)

  gr = cppRouting::makegraph(df, directed = FALSE)

  nds = funique(c(df$from, df$to))

  n = length(nds)

  D = cppRouting::get_distance_matrix(gr, from = nds, to = nds)

  x_init = matrix(rnorm(n*2), ncol = 2)

  stress_layout(x_init, D, iter = 10, tol = 1e-3)

}


#' Plot dependency graph
#' @description Plot a package's dependency graph, coloring each node by the
#'   number of packages it depends on.
#' @param pkg package string passed to \code{\link[pak:pkg_deps]{pak::pkg_deps}}
#' @param dep_type type(s) of dependencies to look up. Valid values are
#'   \code{c("depends", "imports", "linkingto", "suggests")}
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
#' @returns a ggplot
#' @import collapse
#' @examples
#' # Using pkgcache in examples is not allowed, uncomment to run these interactively:
#' # plot_deps_graph("ggplot2") # ggplot2 has many downstream dependencies
#' # plot_deps_graph("rlang") # rlang has only one
#'
#' @export
plot_deps_graph = function(pkg,
                           dep_type = c("depends", "imports", "linkingto"),
                           n_iter = 100,
                           pak_res = NULL,
                           gg = FALSE,
                           lwd = 1,
                           cex = 1,
                           pad_h = .09,
                           pad_w = .08,
                           arw = 1,
                           log_col_scale = FALSE) {

  if (gg) {
    rlang::check_installed(c("ggplot2", "ggraph"))
    cli::cli_abort("ggplot2 version not copied over yet")
  }

  if (log_col_scale) {
    cli::cli_abort("log_col_scale not implemented yet!")
  }

  rlang::arg_match(dep_type,
                   values = c("depends", "imports", "suggests", "linkingto"),
                   multiple = TRUE)

  prgc = get_pkg_graph(pkg, dep_type, pak_res)

  pak_res = prgc[[1]]

  edge_vec = prgc[[2]]

  # This should handle pkg = "." I think?
  if (pkg != sbt(pak_res, direct)$ref[1]) pkg = sbt(pak_res, direct)$package[1]

  # TODO: implement igraph::neighborhood_size
  # ec = igraph::ecount(gr)
  evt = t(edge_vec)

  # TODO: strip repo owner names from pkg
  if (grepl("\\/", pkg)) pkg = pak_res |> sbt(direct) |> getElement('package')

  ns_df = get_deps_memo(evt[, 1], evt[, 2], pkg)

  df = data.frame(from = edge_vec[1,],
                  to = edge_vec[2,],
                  cost = 1)

  nds = funique(c(df$from, df$to))

  n = length(nds)

  gr = cppRouting::makegraph(df, directed = FALSE)

  D = cppRouting::get_distance_matrix(gr, from = nds, to = nds) # yep this is lighter

  x_init = matrix(rnorm(n*2), ncol = 2) # TODO do something better?

  layout_mat = stress_layout(x_init, D, n_iter, 1e-3) # TODO make configurable

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
                 cex = cex
                 )

}

gg_pkg_graph = function(pkg, gr, dep_type,
                        lwd, pad_h, pad_w, cex) {
  # TODO call this fn, pass in args
  ec = igraph::ecount(gr)

  if (ec > 0 ) {
    edges_geom = ggraph::geom_edge_link(arrow = grid::arrow(length = grid::unit(1.5, "mm"),
                                                            type = "closed"),
                                        ggplot2::aes(end_cap = ggraph::label_rect(node2.name)),
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

  gr |>
    ggraph::ggraph(ifelse(ec > 1, "stress", "tree")) +
    edges_geom +
    ggraph::geom_node_label(fill_aes) +
    fill_scale +
    ggplot2::theme_dark() +
    ggplot2::theme(axis.title        = ggplot2::element_blank(),
                   axis.text         = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   plot.background   = ggplot2::element_rect(fill = "#444444"),
                   panel.grid        = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = "#666666"),
                   legend.ticks      = ggplot2::element_line(colour = "#333333")) +
    ggplot2::labs(fill = "n_deps")

}

# get_region = function(th, p2_th, pm_th, pmp2_th) {
#
# }

draw_pkg_graph = function(plot_df, evt, pkg, lwd,
                          pad_h = pad_h,
                          pad_w = pad_w,
                          cex = cex) {


# par ---------------------------------------------------------------------

  par(bg = "grey41",
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


  li = floor(seq(1,100, length.out = 30))
  labs = seq(1, max(plot_df$n_deps), length.out = 4) |>
    floor() |>
    as.character()
  lcols = parula[li]
  ly = yr[1] + .75*diff(yr) + (1:30 / 30 * .2*diff(yr))
  lye = ly + (1 / 30 * .2*diff(yr))
  lx = xr[1] + .9*diff(xr)
  lxe = lx + .02*diff(xr)


  rect(lx, ly, lxe, lye, col = lcols, border = rgb(0,0,0,0))
  text(cex = .85,
       labels = "# deps",
       x = lx,
       y = max(lye) ,
       pos = 3,
       offset = .3,
       col = "grey94")

  text(lx, ly[c(1,10,20,30)],
       labels = labs,
       pos = 2,
       offset = .2,
       col = "grey94",
       cex = .5)

  # title -------------------------------------------------------------------

  par(adj = 0)

  title(pkg, col.main = "grey94")

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

    #TODO: make border red if it's a direct dependency of the top-level one.
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

# get_region = function(dxmm, dymm, h2, w2) {
#   ifelse(dymm < 0 && abs(dymm) > h2,
#          ifelse(),
#          )
#   if () { # 1,2,3
#     if (dxmm > 0 && abs(dxmm) > w2) {
#       3
#     } else if (dxmm < 0 && abs(dxmm) > w2) {
#       1
#     } else {
#       2
#     }
#   } else if (dymm > 0 && abs(dymm) > h2) { # 6, 7, 8
#     NA
#   } else {
#     NA
#   }
#
# }

get_pkg_graph = function(pkg, dep_type, pak_res) {

  dep_type = c("depends", "imports", "linkingto")

  pak_res = pak_res %||% pak::pkg_deps(pkg)

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

