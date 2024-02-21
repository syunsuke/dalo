
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dalo

<!-- badges: start -->
<!-- badges: end -->

The goal of dalo is to download data easily from website.

dalo is **DA**ta down**LO**ad helper tool.

## Installation

You can install the development version of dalo from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("syunsuke/dalo")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dalo)
## basic example code

target <- c("https://syunsuke.github.io/assets/images/001.jpg",
            "https://syunsuke.github.io/assets/images/002.jpg")

dalo_download_by_urls(target, "dl_image/", make_dir = TRUE, check = FALSE)
```
