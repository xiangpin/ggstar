<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggstar: star layer for ggplot2

# :writing\_hand: Author

[Shuangbin Xu](https://github.com/xiangpin)

School of Basic Medical Sciences, Southern Medical University

# :arrow\_double\_down: Installation

``` r
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
devtools::install_github("xiangpin/MicrobiotaProcess")
```

# :beginner: Usage

``` r
library(ggplot2)
library(ggstar)
p <- ggplot(data=mtcars, 
            mapping=aes(x=wt, 
                        y=mpg, 
                        fill=cyl)) + 
     geom_star()
p
```

![](https://github.com/xiangpin/ggstar/blob/master/inst/extdata/figure1.png)

``` r
p2 <- ggplot(data=iris, 
             mapping=aes(x=Sepal.Length, 
                         y=Sepal.Width, 
                         fill=Species)) +
     geom_star()
p2
```

![](https://github.com/xiangpin/ggstar/blob/master/inst/extdata/figure2.png)
