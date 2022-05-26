#' Half Violin plot split across a binary variable
#'
#' A violin plot is a compact display of a continuous distribution. It is a
#' blend of [geom_boxplot()] and [geom_density()]: a
#' violin plot is a mirrored density plot displayed in the same way as a
#' boxplot.
#'
#' @inheritParams geom_half_violin
#' @importFrom ggplot2 layer
#' @examples
#' ggplot() +
#'   geom_half_violin_split(
#'     data = ToothGrowth, 
#'     aes(x = as.factor(dose), y = len, split = supp, fill = supp)
#'   )
#' @export
#' @references Hintze, J. L., Nelson, R. D. (1998) Violin Plots: A Box
#' Plot-Density Trace Synergism. The American Statistician 52, 181-184.
geom_half_violin_split <- function(
  mapping = NULL, data = NULL,
  stat = "half_ydensity_split", position = "dodge",
  ...,
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
    geom = GeomHalfViolinSplit,
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
#' @importFrom ggplot2 ggproto
#' @export
GeomHalfViolinSplit <- ggproto(
  "GeomHalfViolinSplit", GeomHalfViolin,
  
  setup_data = function(data, params) {
    stopifnot(length(unique(data$split)) == 2)
    data <- GeomHalfViolin$setup_data(data, params)
    sides <- c("l", "r")
    split_names <- levels(as.factor(data$split))
    split_names <- factor(split_names, levels = split_names)
    data$side <- sides[match(as.factor(data$split), split_names)]
    
    max_group <- max(data$group) * 2
    data$group <- seq(1, max_group, by = 2)[data$group]
    data$group <- data$group + (data$side == "r")
    data
  },
  
  draw_group = function(self, data, side = "l", nudge = 0, ..., draw_quantiles = NULL) {
    GeomHalfViolin$draw_group(
      data = data, 
      side = data$side[1],
      nudge = nudge,
      ...,
      draw_quantiles = draw_quantiles
    )
  }
)