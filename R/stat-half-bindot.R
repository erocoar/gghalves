#' @rdname gghalves-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto StatBoxplot StatBindot
#' @export
StatHalfBindot <- ggproto(
  "StatHalfBindot", StatBoxplot,
  required_aes = "y",
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    bd_params <- StatBindot$setup_params(data, params)
    bd_params$width <- params$width %||% (resolution(data$x %||% 0) * 0.75)
    bd_params
  },
  
  compute_group = function(
    self, data, scales, binwidth = NULL, binaxis = "y",
    method = "dotdensity", binpositions = "bygroup",
    origin = NULL, width = NULL, drop = FALSE,
    right = TRUE) {

    bin_data <- StatBindot$compute_group(
      data, scales, binwidth = binwidth,
      binaxis = "y", method = method,
      binpositions = binpositions,
      origin = origin, width = width, drop = drop,
      right = right)

    bin_data$width = width
    bin_data
  }
)