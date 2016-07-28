<!-- README.md is generated from README.Rmd. Please edit that file -->
**dirichlet**
=============

**dirichlet** implements the `(d/r)` statistics functions for the [Dirichlet distribution](https://en.wikipedia.org/wiki/Dirichlet_distribution) in [R](http://cran.r-project.org). It is ideal for using in other packages since it is light weight.

### Getting **dirichlet**

<!-- There are two ways to get __chi__.  For the [CRAN version](https://cran.r-project.org/package=chi), use -->
<!-- ```{r, eval=FALSE} -->
<!-- install.packages("chi") -->
<!-- ``` -->
<!-- For the development version, use -->
**dirichlet** is not yet on CRAN, so to get it use the following:

``` r
# install.packages("devtools")
devtools::install_github("dkahle/dirichlet")
```

### The `ddirichlet()` function

The [PDF](https://en.wikipedia.org/wiki/Probability_density_function) (the *f(x)*) can be evaluated with the `ddirichlet()` function:

``` r
library(dirichlet)
ddirichlet(c(.5,.5), c(.5, .5))
#  [1] 0.6366198
```

You can visualize it in barycentric coordinates like this:

``` r
library(dplyr)
#  
#  Attaching package: 'dplyr'
#  
#  The following objects are masked from 'package:stats':
#  
#      filter, lag
#  
#  The following objects are masked from 'package:base':
#  
#      intersect, setdiff, setequal, union
library(ggplot2); theme_set(theme_bw())
f <- function(v) ddirichlet(v, c(20, 10, 5))
mesh <- simplex_mesh(.0025) %>% as.data.frame %>% tbl_df
mesh$f <- mesh %>% apply(1, function(v) f(bary2simp(v)))
  
(p <- ggplot(mesh, aes(x, y)) +
  geom_raster(aes(fill = f)) +
  coord_equal(xlim = c(0,1), ylim = c(0, .85)))
```

![](figures/README-unnamed-chunk-4-1.png)

Random number generation can be performed with `rdirichlet()`:

``` r
set.seed(1)
rdirichlet(3, c(1, 1, 1))  # rows sum to 1
#             [,1]      [,2]       [,3]
#  [1,] 0.07830127 0.4220266 0.49967217
#  [2,] 0.55164885 0.3582736 0.09007756
#  [3,] 0.59019469 0.3788586 0.03094669
rowSums(rdirichlet(3, c(1, 1, 1)))
#  [1] 1 1 1
```

You can visualize these points on top of the distribution above like this:

``` r
points <- rdirichlet(1e3, c(20, 10, 5)) %>% simp2bary %>% 
  as.data.frame %>% tbl_df %>% rename(x = V1, y = V2)

p + geom_point(data = points, color = "orange", alpha = .3)
```

![](figures/README-unnamed-chunk-6-1.png)
