#' Points with jitter for half geoms. Unlike `geom_half_point`, `geom_half_point_panel` does not dodge different grouping aesthetics. This allows multiple groups in a single cloud of points (see examples).
#'
#' @inheritParams geom_half_point
#' @importFrom ggplot2 layer
#' @examples 
#' ggplot(iris, aes(x = Species, y = Petal.Width, color = Species)) + 
#'   geom_half_point_panel()
#'   
#' ggplot(iris, aes(x = Species, y = Petal.Width, color = Species)) + 
#'   geom_half_point_panel(side = "l")
#' @export
geom_half_point_panel <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,
  side = "r",
  transformation = PositionJitter,
  transformation_params = list(width = NULL, height = NULL),
  range_scale = .5,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHalfPointPanel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      side = side,
      transformation = transformation,
      transformation_params = transformation_params,
      range_scale = range_scale,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname gghalves-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom GeomPoint 
#' @export
GeomHalfPointPanel <- ggproto(
  "GeomHalfPointPanel", 
  GeomPoint,
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
    GeomHalfPoint$setup_data(data, params)
  },
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE,
                        side = "r", 
                        transformation = PositionJitter,
                        transformation_params = list(width = NULL, height = NULL),
                        range_scale = .5) {

    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    
    xrange <- data$xmax[1] - data$xmin[1]
    
    x_add  <- (xrange / 4) * switch((side == "r") + 1, -1, 1)
    data$x <- data$x + x_add
    
    # Add Position Transformation
    transformation_df <- data.frame(
      x     = data$x,
      y     = data$y,
      PANEL = 1,
      group = -1
    )
    
    if (is(transformation, "PositionJitter")) {
      transformation_params$width  <- transformation_params$width %||% xrange / (4 / range_scale)
      transformation_params$height <- transformation_params$height %||% 
        ggplot2::resolution(data$y, zero = FALSE) * 0.4
    }

    if (is(transformation, "PositionIdentity") || is(transformation, "PositionJitter")) {
      trans_positions <- transformation$compute_layer(
        transformation_df,
        transformation_params
      )
    } else {
      trans_positions <- transformation$compute_panel(
        transformation_df,
        transformation_params
      )
    }

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
    
    coords <- coord$transform(point_df, panel_params)
    ggplot2:::ggname("geom_half_point_panel",
           pointsGrob(
             coords$x, coords$y,
             pch = coords$shape,
             gp = gpar(
               col = alpha(coords$colour, coords$alpha),
               fill = alpha(coords$fill, coords$alpha),
               # Stroke is added around the outside of the point
               fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
               lwd = coords$stroke * .stroke / 2
             )
           )
    )
  }
)
