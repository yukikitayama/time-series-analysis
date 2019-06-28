GARCH model of S&P500
================

Setup
=====

``` r
library(PerformanceAnalytics)
library(rugarch)
library(xts)
```

Data
====

``` r
sp_df <- read.csv("spx.csv", head = T)
dates <- as.Date(sp_df$date, format = "%d-%b-%y")
sp_xts <- xts(x = sp_df$close,
              order.by = dates)
colnames(sp_xts) <- "close"
head(sp_xts)
```

    ##             close
    ## 1986-01-02 209.59
    ## 1986-01-03 210.88
    ## 1986-01-06 210.65
    ## 1986-01-07 213.80
    ## 1986-01-08 207.97
    ## 1986-01-09 206.11

Transformation
==============

``` r
sp <- CalculateReturns(sp_xts)[-1]
head(sp)
```

    ##                    close
    ## 1986-01-03  0.0061548738
    ## 1986-01-06 -0.0010906677
    ## 1986-01-07  0.0149537147
    ## 1986-01-08 -0.0272684752
    ## 1986-01-09 -0.0089435976
    ## 1986-01-10 -0.0007277667

EDA
===

``` r
hist(sp, nclass = 50)
```

![](GARCH_model_sp500_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
summary(sp)
```

    ##      Index                close           
    ##  Min.   :1986-01-03   Min.   :-0.2046693  
    ##  1st Qu.:1994-02-05   1st Qu.:-0.0043270  
    ##  Median :2002-03-26   Median : 0.0005923  
    ##  Mean   :2002-03-26   Mean   : 0.0003775  
    ##  3rd Qu.:2010-05-12   3rd Qu.: 0.0056006  
    ##  Max.   :2018-06-29   Max.   : 0.1158004

``` r
# Compute daily standard deviation
sd(sp)
```

    ## [1] 0.01132539

``` r
# Compute annualized standard deviation
sqrt(252) * sd(sp)
```

    ## [1] 0.179785
