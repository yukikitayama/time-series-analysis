GARCH model of TEPCO
================

Setup
=====

``` r
# library(RFinanceYJ)
library(quantmod)
```

Data
====

``` r
# Fail
# tepco <- quoteStockXtsData("9984")

# Success
# 1951-08-24: Date listed on Tokyo Stock Exchange from Wiki
# getSymbols("9501",
#            src = "yahooj",
#            from = "1951-08-24", 
#            to = "2019-05-31")
# saveRDS(YJ9501, "YJ9501_19830104_20190531.rds")

tepco <- readRDS("YJ9501_19830104_20190531.rds")
class(tepco)
```

    ## [1] "xts" "zoo"

``` r
head(tepco)
```

    ##            YJ9501.Open YJ9501.High YJ9501.Low YJ9501.Close YJ9501.Volume
    ## 1983-01-04         980         989        976          980        885400
    ## 1983-01-05         989         990        981          985       1091100
    ## 1983-01-06         990        1070        985         1050       7665101
    ## 1983-01-07        1060        1070       1040         1050       7147301
    ## 1983-01-08        1060        1080       1050         1070       4588801
    ## 1983-01-10        1070        1070       1040         1050       2034200
    ##            YJ9501.Adjusted
    ## 1983-01-04          941.85
    ## 1983-01-05          946.66
    ## 1983-01-06         1009.13
    ## 1983-01-07         1009.13
    ## 1983-01-08         1028.35
    ## 1983-01-10         1009.13

``` r
tail(tepco)
```

    ##            YJ9501.Open YJ9501.High YJ9501.Low YJ9501.Close YJ9501.Volume
    ## 2019-05-24         603         608        598          598       5979900
    ## 2019-05-27         598         607        598          606       3576000
    ## 2019-05-28         606         609        597          601      11195900
    ## 2019-05-29         585         589        575          582       7807000
    ## 2019-05-30         574         580        570          570       6320900
    ## 2019-05-31         565         565        553          558       8947600
    ##            YJ9501.Adjusted
    ## 2019-05-24             598
    ## 2019-05-27             606
    ## 2019-05-28             601
    ## 2019-05-29             582
    ## 2019-05-30             570
    ## 2019-05-31             558

EDA
===

``` r
plot.zoo(tepco$YJ9501.Close)
```

![](GARCH_model_TEPCO_files/figure-markdown_github/unnamed-chunk-3-1.png)
