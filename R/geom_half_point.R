#' Points with jitter for half geoms.
#'
#' @inheritParams ggplot2::geom_point
#' @param side The side on which to draw the half violin plot. "l" for left, "r" for right, defaults to "l".
#' @param transformation An evaluated `position_*()` function yielding a `Position` object with specified parameters to calculate the transformation of the points. Defaults to `ggplot2::position_jitter()`.
#' @param range_scale If no `width` argument is specified in `transformation`, `range_scale` is used to determine the width of the jitter. Defaults to `0.75`, which is half of the allotted space for the jitter-points, whereas `1` would use all of the allotted space.
#' @importFrom ggplot2 layer position_jitter .pt .stroke
#' @importFrom grid pointsGrob gpar
#' @examples 
#' ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) + 
#'   geom_half_point()
#'   
#' ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) + 
#'   geom_half_point(side = "l")
#' @export
geom_half_point <- function(
  mapping = NULL, data = NULL,
  stat = "HalfPoint", position = "dodge2",
  ...,
  side = "r",
  transformation = position_jitter(),
  # transformation_params = list(width = NULL, height = NULL),
  range_scale = .75,
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
        # transformation_params = transformation_params,
        range_scale = range_scale,
        na.rm = na.rm,
        ...
      )
    )
}

#' @rdname gghalves-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Geom GeomBoxplot GeomPoint alpha .pt .stroke
#' @importFrom grid pointsGrob gpar
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
    transformation = position_jitter(),
    # transformation_params = list(width = NULL, height = NULL),
    range_scale = .75) {

    # if (isFALSE(isTRUE(all.equal(transformation_params, list(width = NULL, height = NULL))))) {
    #   warning("Argument deprecated.\n Use `transformation = position_*(params)` instead of passing the params via `transformation_params`")
    # }
    
    if (is.character(data$shape)) {
      data$shape <- ggplot2:::translate_shape_string(data$shape)
    }
    
    xrange <- data$xmax - data$xmin
    x_add  <- (xrange / 4) * switch((side == "r") + 1, -1, 1)
    data$x <- data$x + x_add

    # Add Position Transformation
    transformation_params_new <- transformation$setup_params(data)
    transformation_df <- data.frame(
      x     = data$x,
      y     = data$point_y[[1]],
      PANEL = 1,
      group = -1
    )
    
    
    null_width <- FALSE
    name_width <- "width" %in% names(transformation)
    if (isTRUE(name_width)) {
      null_width <- is.null(transformation[["width"]])
    }
    if ((!name_width || null_width) && "width" %in% names(transformation_params_new)) {
      transformation_params_new$width <- xrange / (4 / range_scale)
    }
    
    null_height <- FALSE
    name_height <- "height" %in% names(transformation)
    if (isTRUE(name_height)) {
      null_height <- is.null(transformation[["height"]])
    }
    if ((!name_height || null_height) && "height" %in% names(transformation_params_new)) {
      transformation_params_new$height <- ggplot2::resolution(data$point_y[[1]], zero = FALSE) * 0.4
    }
    
    # Add deprecated transformation_params args to new transformation_params list
    # par_idx <- !sapply(transformation_params, is.null)
    # transformation_params_new[names(transformation_params)[par_idx]] <- transformation_params[par_idx]
    # transformation_params <- transformation_params_new
    # 
    # # deprecated
    # if ("width" %in% names(transformation_params)) {
    #   transformation_params$width <- transformation_params$width %||% xrange / (4 / range_scale)
    # }
    # # deprecated
    # if ("height" %in% names(transformation_params)) {
    #   transformation_params$height <- transformation_params$height %||% 
    #     ggplot2::resolution(data$point_y[[1]], zero = FALSE) * 0.4
    # }

    # if (is(transformation, "PositionJitter")) {
    #   transformation_params$width  <- transformation_params$width %||% xrange / (4 / range_scale)
    #   transformation_params$height <- transformation_params$height %||% 
    #     ggplot2::resolution(data$point_y[[1]], zero = FALSE) * 0.4
    # }

    if (is(transformation, "PositionIdentity") || is(transformation, "PositionJitter")) {
      trans_positions <- transformation$compute_layer(
        transformation_df,
        transformation_params_new
      )
    } else {
      trans_positions <- transformation$compute_panel(
        transformation_df,
        transformation_params_new
      )
    }

    # if (length(unique(trans_positions$x)) > 1L) {
    #   if (side == "r") {
    #     trans_positions$x <- (data$xmax - data$x) * (
    #       trans_positions$x - min(trans_positions$x)) / (
    #         max(trans_positions$x) - min(trans_positions$x)) + data$x #- 0.045
    #   } else {
    #     trans_positions$x <- (data$x - data$xmin) * (
    #       trans_positions$x - min(trans_positions$x)) / (
    #         max(trans_positions$x) - min(trans_positions$x)) + data$xmin #+ 0.045
    #   }
    # } #TODO parameterize left/right-shift
      

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
    ggplot2:::ggname(
      "geom_half_point",
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
