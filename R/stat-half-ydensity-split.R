#' @inheritParams stat_half_ydensity
#' @seealso [geom_half_violin_split()] for examples.
#' @importFrom ggplot2 layer
#' @export
#' @rdname geom_half_violin_split
stat_half_ydensity_split <- function(
  mapping = NULL, data = NULL,
  geom = "half_violin_split", position = "dodge",
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
    stat = StatHalfYdensitySplit,
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
#' @importFrom ggplot2 ggproto
#' @export
StatHalfYdensitySplit <- ggproto(
  "StatHalfYdensitySplit", StatHalfYdensity,
  required_aes = c("x", "y", "split"),
  non_missing_aes = "weight"
)
