StatHalfBindot <- ggproto(
  "StatHalfBindot", StatBoxplot,
  required_aes = "y",
  non_missing_aes = "weight",
  default_aes = aes(y = stat(count)),
  
  setup_params = function(data, params) {
    StatBindot$setup_params(data, params)
    },
  
  compute_group = function(
    self, data, scales, binwidth = NULL, binaxis = "y",
    method = "dotdensity", binpositions = "bygroup",
    origin = NULL, width = 0.9, drop = FALSE,
    right = TRUE) {
    StatBindot$compute_group(data, scales, binwidth = binwidth,
                             binaxis = "y", method = method,
                             binpositions = binpositions,
                             origin = origin, width = width, drop = drop,
                             right = right)
    }
)