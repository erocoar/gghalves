#' A half boxplot
#'
#' @inheritParams ggplot2::geom_boxplot
#' @param errorbar.draw Draw horizontal whiskers at the top and bottom (the IQR). Defaults to `TRUE`.
#' @param errorbar.length Length of the horizontal whiskers (errorbar). Defaults to half the width of the half-boxplot
#' @param side The side of the half-geom, "l" for left and "r" for right, defaults to "l".
#' @param center Boolean whether to center the half-boxplot instead of aligning it to its respective side.
#' @param nudge Add space between the boxplot and the middle of the space allotted to a given factor on the x-axis.
#' @importFrom ggplot2 layer position_dodge2 aes GeomSegment GeomCrossbar
#' @importFrom grid grobTree grobName
#' @examples 
#' ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) + 
#'   geom_half_boxplot() 
#'   
#' ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) + 
#'   geom_half_boxplot(side = "r") 
#'   
#' ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) + 
#'   geom_half_boxplot(center = TRUE) 
#' @export
geom_half_boxplot <- function(
  mapping = NULL, data = NULL,
  stat = "boxplot", position = "dodge2",
  ...,
  side = "l",
  center = FALSE,
  nudge = 0,
  outlier.colour = NULL,
  outlier.color = NULL,
  outlier.fill = NULL,
  outlier.shape = 19,
  outlier.size = 1.5,
  outlier.stroke = 0.5,
  outlier.alpha = NULL,
  notch = FALSE,
  notchwidth = 0.5,
  varwidth = FALSE,
  errorbar.draw = TRUE,
  errorbar.length = 0.5,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      warning("Can't preserve total widths when varwidth = TRUE.", call. = FALSE)
      position$preserve <- "single"
    }
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHalfBoxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      side = side,
      center = center,
      nudge = nudge,
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      errorbar.draw = errorbar.draw,
      errorbar.length = errorbar.length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname gghalves-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 alpha ggproto GeomBoxplot aes GeomSegment GeomPoint GeomCrossbar resolution PositionJitter
#' @importFrom grid grobTree
#' @export
GeomHalfBoxplot <- ggproto("GeomHalfBoxplot", GeomBoxplot,
  setup_data = function(data, params) {
    GeomBoxplot$setup_data(data, params)
  },
                           
  draw_group = function(
    data, panel_params, coord, fatten = 2,
    side = "l", center = FALSE, nudge = nudge,
    outlier.colour = NULL, outlier.fill = NULL,
    outlier.shape = 19, outlier.size = 1.5, 
    outlier.stroke = 0.5, outlier.alpha = NULL,
    notch = FALSE, notchwidth = 0.5, 
    varwidth = FALSE, errorbar.draw = FALSE, errorbar.length = 0.5) {
    
    if (nrow(data) != 1) {
      stop(
        "Can't draw more than one boxplot per group. Did you forget aes(group = ...)?",
        call. = FALSE
      )
    }
    
    xrange <- data$xmax - data$xmin
    
    common <- data.frame(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE
    )
                           
    whiskers <- data.frame(
       x = data$x,
       xend = data$x,
       y = c(data$upper, data$lower),
       yend = c(data$ymax, data$ymin),
       alpha = NA,
       common,
       stringsAsFactors = FALSE
    )
    
    # Adjust whisker position based on nudge (extra spacing between geom and middle)
    # If side == right, move whisker to right and vice versa
    if (side == "r") {
      whiskers$x <- whiskers$x + nudge
    } else {
      whiskers$x <- whiskers$x - nudge
    }
    whiskers$xend <- whiskers$x
    
    # If boxplot is centered, need to adjust whisker that is otherwise always at x
    # If boxplot is centered, only half of nudge value is added s.t. it remains centered
    if (isTRUE(center)) {
      if (side == "r") {
        whiskers$x <- data$x + xrange / 4 + nudge / 2
      } else {
        whiskers$x <- data$x - xrange / 4 - nudge / 2
      }
      whiskers$xend <- whiskers$x
    }
    
    if (errorbar.draw) {
      if (errorbar.length > 1 | errorbar.length < 0) {
        stop("Error bar length must be between 0 and 1.")
      }
      error_length_add <- xrange / 2 #((data$xmin + xrange / 2) - data$xmin)
      error_length_add <- error_length_add * (1 - errorbar.length)
  
      error_whiskers <- data.frame(
        x = if (side == "r") (data$xmin + xrange / 2) + nudge else (data$xmin + xrange / 2) - nudge ,
        xend = if (side == "r") data$xmax - error_length_add + nudge else data$xmin + error_length_add - nudge,
        y = c(data$ymax, data$ymin),
        yend = c(data$ymax, data$ymin),
        alpha = NA,
        common,
        stringsAsFactors = FALSE
        )
      
      if (isTRUE(center)) {
        error_whiskers$x <- data$x
        if (side == "r") {
          error_whiskers$xend <- data$xmax
        } else {
          error_whiskers$xend <- data$xmin
        }
      }
                               
      error_grob <- GeomSegment$draw_panel(error_whiskers, panel_params, coord)
      } else {
        error_grob <- NULL
      }
    
    box <- data.frame(
      xmin = if (side == "r") data$xmax else data$xmin,
      xmax = (data$xmin + xrange / 2) + switch((side == "r") + 1, nudge * -1, nudge),
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha,
      common,
      stringsAsFactors = FALSE
      )
    
    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- ggplot2:::new_data_frame(list(
        y = data$outliers[[1]],
        x = data$x[1] + switch((side == "r") + 1, nudge * -1, nudge),
        colour = outlier.colour %||% data$colour[1],
        fill = outlier.fill %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = outlier.alpha %||% data$alpha[1]
      ), n = length(data$outliers[[1]]))
      
      if (isTRUE(center)) {
        if (side == "r") {
          outliers$x <- outliers$x + xrange / 4
        } else {
          outliers$x <- outliers$x - xrange / 4
        }
      }
      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }
    
    tree <- grobTree(
      outliers_grob,
      error_grob,
      GeomSegment$draw_panel(whiskers, panel_params, coord),
      GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord)
  )
  tree$name <- grid::grobName(tree, "geom_half_boxplot")
  tree
  }
)
