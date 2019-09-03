#' Points with jitter for half geoms.
#'
#' @inheritParams ggplot2::geom_boxplot
#' @importFrom ggplot2 layer position_dodge2 new_data_frame aes
#' @export
#' @examples

geom_half_point <- function(
  mapping = NULL, data = NULL,
  stat = "HalfPoint", position = "dodge2",
  ...,
  side = "r",
  transformation = PositionJitter,
  transformation_params = list(width = NULL, height = NULL),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomHalfPoint,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        side = side,
        transformation = transformation,
        transformation_params = transformation_params,
        na.rm = na.rm,
        ...
      )
    )
}

#' @export
GeomHalfPoint <- ggproto(
  "GeomHalfPoint", 
  Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, 
    colour = "black", 
    size = 1.5, 
    fill = NA,
    alpha = NA, 
    stroke = 0.5
    ),
  
  setup_data = function(data, params) {
    x_data    <- GeomBoxplot$setup_data(data, NULL)
    data$xmin <- x_data$xmin
    data$xmax <- x_data$xmax
    data
  },

  draw_group = function(
    data, panel_params, coord, na.rm = FALSE, side = "r", 
    transformation = PositionJitter,
    transformation_params = list(width = NULL, height = NULL)) {
    ddn <<- data
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    
    xrange <- data$xmax - data$xmin
    x_add  <- (xrange / 4) * switch((side == "r") + 1, -1, 1)
    data$x <- data$x + x_add
    
    # Add Position Transformation
    transformation_df <- data.frame(
      x     = data$x,
      y     = data$point_y[[1]],
      PANEL = 1,
      group = -1
    )
    
    if (is(transformation, "PositionJitter")) {
      transformation_params$width  <- transformation_params$width %||% xrange / 8
      transformation_params$height <- transformation_params$height %||% 
        ggplot2::resolution(data$point_y[[1]], zero = FALSE) * 0.4
    }

    trans_positions <- transformation$compute_layer(
      transformation_df,
      transformation_params
    )

    if (length(unique(trans_positions$x)) > 1L) {
      trans_positions$x <- (data$xmax- data$x) * (
        trans_positions$x - min(trans_positions$x)) / (
          max(trans_positions$x) - min(trans_positions$x)) + data$x - 0.045
    }#TODO parameterize left-shift
      

    point_df <- data.frame(
      colour = data$colour,
      shape  = data$shape,
      x      = trans_positions$x,
      y      = trans_positions$y,
      PANEL  = data$PANEL,
      group  = data$group,
      size   = data$size,
      fill   = data$fill,
      alpha  = data$alpha,
      stroke = data$stroke
    )
    
    GeomPoint$draw_panel(point_df, panel_params, coord)
    # pointgrob <- pointsGrob(
    #   coords$x, coords$y,
    #   pch = coords$shape,
    #   gp = gpar(
    #     col = alpha(coords$colour, coords$alpha),
    #     fill = alpha(coords$fill, coords$alpha),
    #     fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
    #     lwd = coords$stroke * .stroke / 2
    #   )
    # )
    # pointgrob$name <- "geom_half_point"
    # pointgrob
  }
)
