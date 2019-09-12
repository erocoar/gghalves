#' Half dot plot with sensible parameter settings.
#'
#' In a dot plot, the width of a dot corresponds to the bin width
#' (or maximum width, depending on the binning algorithm), and dots are
#' stacked, with each dot representing one observation.
#'
#' There are two basic approaches: \emph{dot-density} and \emph{histodot}.
#' With dot-density binning, the bin positions are determined by the data and
#' `binwidth`, which is the maximum width of each bin. See Wilkinson
#' (1999) for details on the dot-density binning algorithm. With histodot
#' binning, the bins have fixed positions and fixed widths, much like a
#' histogram.
#'
#' When binning along the x axis and stacking along the y axis, the numbers on
#' y axis are not meaningful, due to technical limitations of ggplot2. You can
#' hide the y axis, as in one of the examples, or manually scale it
#' to match the number of dots.
#'
#' @section Computed variables:
#' \describe{
#'   \item{x}{center of each bin, if binaxis is "x"}
#'   \item{y}{center of each bin, if binaxis is "x"}
#'   \item{binwidth}{max width of each bin if method is "dotdensity";
#'     width of each bin if method is "histodot"}
#'   \item{count}{number of points in bin}
#'   \item{ncount}{count, scaled to maximum of 1}
#'   \item{density}{density of points in bin, scaled to integrate to 1,
#'     if method is "histodot"}
#'   \item{ndensity}{density, scaled to maximum of 1, if method is "histodot"}
#' }
#'
#' @inheritParams ggplot2::geom_dotplot
#' @param stackdir Which direction to stack the dots. "up" (default) places the half-dotplot on the right side. "down" on the left side.
#' @importFrom ggplot2 layer
#' @export
#' @references Wilkinson, L. (1999) Dot plots. The American Statistician,
#'    53(3), 276-281.
geom_half_dotplot <- function(
  mapping = NULL, data = NULL,
  position = "dodge",
  ...,
  binwidth = NULL,
  binaxis = "y",
  method = "dotdensity",
  binpositions = "bygroup",
  stackdir = "up",
  stackratio = 1,
  dotsize = 1,
  stackgroups = FALSE,
  origin = NULL,
  right = TRUE,
  width = NULL,
  drop = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  
  # If identical(position, "stack") or position is position_stack(), tell them
  # to use stackgroups=TRUE instead. Need to use identical() instead of ==,
  # because == will fail if object is position_stack() or position_dodge()
  if (!is.null(position) &&
      (identical(position, "stack") || (inherits(position, "PositionStack"))))
    message("position=\"stack\" doesn't work properly with geom_dotplot. Use stackgroups=TRUE instead.")
  
  if (stackgroups && method == "dotdensity" && binpositions == "bygroup")
    message('geom_dotplot called with stackgroups=TRUE and method="dotdensity". You probably want to set binpositions="all"')
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatHalfBindot,
    geom = GeomHalfDotplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    # Need to make sure that the binaxis goes to both the stat and the geom
    params = list(
      binaxis = binaxis,
      binwidth = binwidth,
      binpositions = binpositions,
      method = method,
      origin = origin,
      right = right,
      width = width,
      drop = drop,
      stackdir = stackdir,
      stackratio = stackratio,
      dotsize = dotsize,
      stackgroups = stackgroups,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname gghalves-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomDotplot aes
#' @export
GeomHalfDotplot <- ggproto(
  "GeomHalfDotplot", GeomDotplot,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape"),
  default_aes = aes(colour = "black", fill = "black", alpha = NA, stroke = 1, linetype = "solid")
)