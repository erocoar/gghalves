[![Build Status](https://travis-ci.org/erocoar/gghalves.svg?branch=master)](https://travis-ci.org/erocoar/gghalves)
[![CRAN_Release_Badge](http://www.r-pkg.org/badges/version-ago/gghalves)](https://CRAN.R-project.org/package=gghalves)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/gghalves)](https://CRAN.R-project.org/package=gghalves)

### About
`gghalves` makes it easy to compose your own `half-half` plots via `ggplot2`. Think displaying a boxplot next to jittered points, or violin plots side by side with dotplots. 

### Installation
`gghalves` can be installed via GitHub:

```r
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('erocoar/gghalves')
```

### Features
`gghalves` adds `_half` extensions to selected `geom`s:
- `geom_half_boxplot`
- `geom_half_violin`
- `geom_half_point`

All of them have a `side` argument that can be either `l` for left or `r` for right half. As a special case there is also `geom_half_dotplot` to support half dotplots that dodge aesthetics such as `fill` and `colour`. 

`gghalves` also works well with other `ggplot2` geoms and extensions such as `geom_dotplot` and `ggbeeswarm::geom_beeswarm`.

![Features](https://i.imgur.com/7APxx1t.png)

### Roadmap
Generally, I hope to add more features over time. If you would like to see a certain feature, please open an issue.
