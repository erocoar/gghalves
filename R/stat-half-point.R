StatHalfPoint <- ggproto(
  "StatHalfPoint", StatBoxplot,
  required_aes = c("y"),
  non_missing_aes = "weight",
  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
    df <- StatBoxplot$compute_group(data, scales, width, na.rm, coef)
    df$point_y      <- list(data$y)
    df$point_colour <- list(data$colour)
    df$point_shape  <- list(data$shape)
    df$point_size   <- list(data$size)
    df$point_fill   <- list(data$fill)
    df$point_alpha  <- list(data$alpha)
    df$point_stroke <- list(data$stroke)
    df$y            <- df$x
    df
  }
)