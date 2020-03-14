<!-- badges: start -->
  [![Build Status](https://travis-ci.com/Zhaoyu-Andrew/Rpackage.svg?branch=master)](https://travis-ci.com/Zhaoyu-Andrew/Rpackage)
  <!-- badges: end -->

<!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/Zhaoyu-Andrew/Rpackage/branch/master/graph/badge.svg)](https://codecov.io/gh/Zhaoyu-Andrew/Rpackage?branch=master)
  <!-- badges: end -->

# Rpackage

Rpackage contains 4 functions for data prediction 

## Installation

To download the **Rpackage** package, use the code below.

```r
# install.packages("devtools")
devtools::install_github("Zhaoyu-Andrew/Rpackage")
library(Rpackage)
```

## Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code 

```r
# install.packages("devtools")
devtools::install_github("Zhaoyu-Andrew/Rpackage", build_vignette = TRUE, build_opts = c())
library(Rpackage)
# Use this to view the vignette in the Rpackage HTML help
help(package = "Rpackage", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "Rpackage")
```
