#' @rdname gghalves-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto StatBoxplot
#' @export
StatHalfPoint <- ggproto(
  "StatHalfPoint", StatBoxplot,
  required_aes = c("y"),
  non_missing_aes = "weight",
  
  setup_data = function(data, params) {
    data$x <- data$x %||% 0
    data <- remove_missing(
      data,
      na.rm = FALSE,
      vars = "x",
      name = "stat_boxplot"
    )
    data
  },
  
  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
    df <- StatBoxplot$compute_group(data, scales, width, na.rm, coef)
    # data.frame(
    #   y      = data$y,
    #   x      = df$x,
    #   width  = df$width %||% NULL,
    #   colour = data$colour ,
    #   shape  = data$shape,
    #   size   = data$size, 
    #   fill   = data$fill,
    #   alpha  = data$alpha,
    #   stroke = data$stroke
    # )
    
    df$point_y      <- list(data$y)
    df$point_colour <- list(data$colour)
    df$point_shape  <- list(data$shape)
    df$point_size   <- list(data$size)
    df$point_fill   <- list(data$fill)
    df$point_alpha  <- list(data$alpha)
    df$point_stroke <- list(data$stroke)
    df$y            <- df$ymax
    df
  }
)
