# gghalves 0.0.2

## Minor changes
- `geom_half_point` now supports passing of positions other than `PositionJitter` and `PositionIdentity` to `transformation` - given they have a `compute_panel` function.
- added `nudge` parameter to `geom_half_boxplot` and `geom_half_violin`. `nudge` takes as value a non-negative scalar and adds space between the middle of the space allotted to a factor on the x-axis and the respective geom.

# gghalves 0.0.1

## Major changes
- First commit

## New features
- `geom_half_boxplot` for half-boxplots.
- `geom_half_dotplot` for aligned dotplots that dodge fill and color.
- `geom_half_violin` for half-violin plots.
- `geom_half_point` for half-jitterplots.