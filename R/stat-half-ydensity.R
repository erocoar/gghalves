#' @inheritParams ggplot2::stat_ydensity
#' @seealso [geom_half_violin()] for examples.
#' @importFrom ggplot2 layer
#' @export
#' @rdname geom_half_violin
stat_half_ydensity <- function(
  mapping = NULL, data = NULL,
  geom = "half_violin", position = "dodge",
  ...,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  trim = TRUE,
  scale = "area",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  
  scale <- match.arg(scale, c("area", "count", "width"))
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatHalfYdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      scale = scale,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname gghalves-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto StatBoxplot StatYdensity
#' @export
StatHalfYdensity <- ggproto(
  "StatHalfYdensity", StatBoxplot,
  required_aes = c("x", "y"),
  non_missing_aes = c("weight", "split"),
  
  compute_group = function(
    data, scales, width = NULL, bw = "nrd0", adjust = 1,
    kernel = "gaussian", trim = TRUE, na.rm = FALSE) {
    StatYdensity$compute_group(
      data, scales, width = width, bw = bw, adjust = adjust,
      kernel = kernel, trim = trim, na.rm = na.rm)
    },
  
  compute_panel = function(
    self, data, scales, width = NULL, bw = "nrd0", adjust = 1,
    kernel = "gaussian", trim = TRUE, na.rm = FALSE, scale = "area") {
    StatYdensity$compute_panel(
      data, scales, width = width, bw = bw, adjust = adjust,
      kernel = kernel, trim = trim, na.rm = na.rm, scale = scale)
  }
)
