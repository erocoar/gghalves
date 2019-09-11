geom_half_dotplot <- function(
  mapping = NULL, data = NULL,
  position = "dodge",
  ...,
  side = "r",
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
  width = 0.9,
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
    stat = "half_bindot",
    geom = GeomHalfDotplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    # Need to make sure that the binaxis goes to both the stat and the geom
    params = list(
      side = side,
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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHalfDotplot <- ggproto(
  "GeomHalfDotplot", GeomBoxplot,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape"),
  
  default_aes = aes(colour = "black", fill = "black", alpha = NA, stroke = 1, linetype = "solid"),
  
  setup_data = function(data, params) {
    x_data    <- GeomBoxplot$setup_data(data, NULL)
    data$bp_xmin <- x_data$xmin
    data$bp_xmax <- x_data$xmax
    GeomDotplot$setup_data(data, params)
  },
  
  draw_group = function(
    data, panel_params, coord, na.rm = FALSE,
    binaxis = "x", stackdir = "up", stackratio = 1,
    dotsize = 1, stackgroups = FALSE, side="r") {
    if (!coord$is_linear()) {
      warning("geom_dotplot does not work properly with non-linear coordinates.")
    }
    
    data$xmin <- if (side == "l") data$bp_xmin else data$bp_xmin + (data$bp_xmax - data$bp_xmin) / 2
    data$xmax <- if (side == "l") data$bp_xmin + (data$bp_xmax - data$bp_xmin) / 2 else data$bp_xmax
    dddx[[length(dddx) + 1]] <<- data
    tdata <- coord$transform(data, panel_params)
    
    # Swap axes if using coord_flip
    if (inherits(coord, "CoordFlip"))
      binaxis <- ifelse(binaxis == "x", "y", "x")
      
    if (binaxis == "x") {
      stackaxis = "y"
      dotdianpc <- dotsize * tdata$binwidth[1] / (max(panel_params$x.range) - min(panel_params$x.range))
      
    } else if (binaxis == "y") {
      stackaxis = "x"
      dotdianpc <- dotsize * tdata$binwidth[1] / (max(panel_params$y.range) - min(panel_params$y.range))
    }
    
    ggplot2:::ggname("geom_dotplot",
           ggplot2:::dotstackGrob(
             stackaxis = stackaxis, x = tdata$x, y = tdata$y, dotdia = dotdianpc,
             stackposition = tdata$stackpos, stackratio = stackratio,
             default.units = "npc",
             gp = gpar(col = alpha(tdata$colour, tdata$alpha),
                       fill = alpha(tdata$fill, tdata$alpha),
                       lwd = tdata$stroke, lty = tdata$linetype))
    )
  }
)
