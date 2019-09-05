#TODO add side arg.
#TODO add stat_violin to get xvals from StatBoxplot
geom_half_violin <- function(
  mapping = NULL, data = NULL,
  stat = "ydensity", position = "dodge",
  ...,
  side = "l",
  draw_quantiles = NULL,
  trim = TRUE,
  scale = "area",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHalfViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHalfViolin <- ggproto(
  "GeomHalfViolin", GeomViolin,
  
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    
    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    ggplot2:::dapply(data, "group", transform,
           xmin = x - width / 2,
           xmax = x 
    )
  },

  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    # Find the points for the line to go all the way around
    data <- transform(
      data,
      xminv = x + violinwidth * (xmin - x),
      xmaxv = x
      )
    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(
      transform(data, x = xminv)[order(data$y), ],
      transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
    )
    
    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    
    # Draw quantiles if requested, so long as there is non-zero y range
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      
      # Compute the quantile segments and combine with existing aesthetics
      quantiles <- create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[
        rep(1, nrow(quantiles)),
        setdiff(names(data), c("x", "y", "group")),
        drop = FALSE
        ]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      both <- both[!is.na(both$group), , drop = FALSE]
      quantile_grob <- if (nrow(both) == 0) {
        zeroGrob()
      } else {
        GeomPath$draw_panel(both, ...)
      }
      
      ggplot2:::ggname("geom_violin", grobTree(
        GeomPolygon$draw_panel(newdata, ...),
        quantile_grob)
      )
    } else {
      ggplot2:::ggname("geom_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)