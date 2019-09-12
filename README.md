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

![Features](https://i.imgur.com/7APxx1t.png)

### Roadmap
Generally, I hope to add more features over time. If you would like to see a certain feature, please open an issue.
