# gghalves 0.1.2
## New Features
- `geom_half_violin()` now accepts an optional `split` aesthetic to compare a binary variable side-by-side (see vignette)
- `geom_half_violin()` now accepts a vector for the `side` argument, supporting different sizes per plotting group

## Bug Fixes
- fixed missing import of `remove_missing` function
- fixed missing import of `translate_shape_string` function


# gghalves 0.1.1
## Minor Changes
- `transformation_params` argument of `geom_half_point` has been removed (after previously having been deprecated). Instead, pass `position_*(params)` with the `transformation` argument instead.
- `range_scale` has been set to a default value of `0.75` for `geom_half_point_panel` to match `geom_half_point`.
## Bug Fixes
- fixed a bug in `geom_half_point` where jitter width defaulted to the default value of `ggplot2::PositionJitter` rather than a custom calculation. `range_scale` should work better as a result, and the geom can be used with small values without problems.

# gghalves 0.1.0

## New features
- `geom_half_point_panel` for jittering points but allowing grouped aesthetics inside that jitter (rather than dodging per group). 

## Major Changes
- `transformation_params` argument of `geom_half_point` has been deprecated in favor of passing the arguments directly to `transformation` by evaluating `position_*(arguments)` functions that return `Position` objects.

## Minor changes
- `geom_half_point` now supports passing of positions other than `PositionJitter` and `PositionIdentity` to `transformation` -- given they have a `compute_panel` function.
- added `range_scale` parameter to `geom_half_point`. When no `width` argument is passed in `transformation_params`, `range_scale` defaults to `0.5`, which leads to the points using half of their allotted space. `1` would have them end up using all of their allotted space. 
- added default `width` and `height` values when `width` or `height` are included names in the `transformation_params` list but set to `NULL`. This leads to sensible defaults for `Position`s that are not `PositionJitter`. 


## Bug Fixes
- removed range scaling of `geom_half_point` after points were already jittered
- fixed y-axis limits being set to # of factor levels when the maximum y-value is lower than the # of factor values
- accurate naming of `geom_half_violin` and `geom_half_point` grobs by drawing internally rather than calling `Geom*$draw_panel`

# gghalves 0.0.1

## Major changes
- First commit

## New features
- `geom_half_boxplot` for half-boxplots.
- `geom_half_dotplot` for aligned dotplots that dodge fill and color.
- `geom_half_violin` for half-violin plots.
- `geom_half_point` for half-jitterplots.