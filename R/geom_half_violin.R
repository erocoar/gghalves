#' Half Violin plot
#'
#' A violin plot is a compact display of a continuous distribution. It is a
#' blend of [geom_boxplot()] and [geom_density()]: a
#' violin plot is a mirrored density plot displayed in the same way as a
#' boxplot.
#' 
#' The half-violin plot accepts an optional `split` aesthethic to compare 
#' data separated by a binary variable side-by-side.
#'
#' @inheritParams ggplot2::geom_violin
#' @param side The side on which to draw the half violin plot. "l" for left, "r" for right, defaults to "l".
#' @param nudge Add space between the violinplot and the middle of the space allotted to a given factor on the x-axis.
#' @importFrom ggplot2 layer
#' @examples
#' ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) +
#'   geom_half_violin()
#'
#' ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) +
#'   geom_half_violin(side = "r")
#'   
#' ggplot() +
#'   geom_half_violin(
#'     data = ToothGrowth, 
#'     aes(x = as.factor(dose), y = len, split = supp, fill = supp),
#'     position = "identity"
#'   ) + 
#'   theme_minimal()  
#' @export
#' @references Hintze, J. L., Nelson, R. D. (1998) Violin Plots: A Box
#' Plot-Density Trace Synergism. The American Statistician 52, 181-184.
geom_half_violin <- function(
  mapping = NULL, data = NULL,
  stat = "half_ydensity", position = "dodge",
  ...,
  side = "l",
  nudge = 0,
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
      side = side,
      nudge = nudge,
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname gghalves-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomViolin GeomBoxplot GeomPolygon
#' @export
GeomHalfViolin <- ggproto(
  "GeomHalfViolin", GeomViolin,
  
  default_aes = ggplot2:::modify_list(aes(split = NA), GeomViolin$default_aes),

  setup_data = function(data, params) {
    x_data    <- GeomBoxplot$setup_data(data, NULL)
    data$xmin <- x_data$xmin
    data$xmax <- x_data$xmax
    data
  },

  setup_params = function(data, params) {
    if ("split" %in% colnames(data)) {
      stopifnot(length(unique(data$split)) == 2)
      params$side <- rep(c("l", "r"), max(data$group) / 2)
    } else {
      params$side <- rep(params$side, 
                       ceiling(length(unique(data$group))/length(params$side)))
    }
    params
  },
  
  draw_group = function(self, data, side = "l", nudge = 0, ..., draw_quantiles = NULL) {
    # Find the points for the line to go all the way around
    if (length(side) == 1) {
      side <- rep(side, data$group[1])
    }
    if (side[unique(data$group)] == "l") {
      data <- transform(
        data,
        xminv = x + violinwidth * (xmin - x) - nudge,
        xmaxv = x - nudge
        )
    } else {
      data <- transform(
        data,
        xminv = x + nudge,
        xmaxv = x + violinwidth * (xmax - x) + nudge
      )
    }

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
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
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

      ggplot2:::ggname("geom_half_violin", grobTree(
        GeomPolygon$draw_panel(newdata, ...),
        quantile_grob)
      )
    } else {
      ggplot2:::ggname("geom_half_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)