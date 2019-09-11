#' A half boxplot
#'
#' @inheritParams ggplot2::geom_boxplot
#' 
#' @param errorbar.draw Draw horizontal whiskers at the top and bottom (the IQR). Defaults to `FALSE`.
#' 
#' @param errorbar.length Length of the horizontal whiskers (errorbar). Defaults to half the width of the half-boxplot.
#' 
#' @importFrom ggplot2 layer position_dodge2 new_data_frame aes
#' @export
#' @examples
#' set.seed(221)
#' df <- data.frame(score = rgamma(150, 4, 1), 
#'                  gender = sample(c("M", "F"), 150, replace = TRUE), 
#'                  genotype = factor(sample(1:3, 150, replace = TRUE)))
#' 
#' ggplot(df) + geom_boxjitter(aes(x = gender, y = score, fill = genotype),
#'                             errorbar.draw = TRUE) +
#'   scale_fill_manual(values = c("#CF3721", "#31A9B8", "#258039")) +
#'   theme_minimal()
geom_half_boxplot <- function(
  mapping = NULL, data = NULL,
  stat = "boxplot", position = "dodge2",
  ...,
  side = "l",
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
  errorbar.draw = FALSE,
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

#' @rdname ggpol-extensions
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
    side = "l",
    outlier.colour = NULL, outlier.fill = NULL,
    outlier.shape = 19, outlier.size = 1.5, 
    outlier.stroke = 0.5, outlier.alpha = NULL,
    jitter.position = ggplot2::PositionJitter,
    jitter.params = list("width" = NULL, "height" = NULL),
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
                           
  if (errorbar.draw) {
    if (errorbar.length > 1 | errorbar.length < 0) {
      stop("Error bar length must be between 0 and 1.")
    }
    error_length_add <- ((data$xmin + xrange / 2) - data$xmin)
    error_length_add <- error_length_add * (1 - errorbar.length)

    error_whiskers <- data.frame(
      x = (data$xmin + xrange / 2),
      xend = if (side == "r") data$xmax - error_length_add else data$xmin + error_length_add,
      y = c(data$ymax, data$ymin),
      yend = c(data$ymax, data$ymin),
      alpha = NA,
      common,
      stringsAsFactors = FALSE
      )
                             
    error_grob <- GeomSegment$draw_panel(error_whiskers, panel_params, coord)
    } else {
      error_grob <- NULL
    }
    
    box <- data.frame(
      xmin = if (side == "r") data$xmax else data$xmin,
      xmax = (data$xmin + xrange / 2),
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
    
    bb <<- box
    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- ggplot2:::new_data_frame(list(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        fill = outlier.fill %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = outlier.alpha %||% data$alpha[1]
      ), n = length(data$outliers[[1]]))
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
