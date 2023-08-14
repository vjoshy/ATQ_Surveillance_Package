
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ATQ: Assesing Evaluation Metrics for Timely Epidemic Detection Models

<!-- badges: start -->
<!-- badges: end -->

The goal of ATQ is to provide accurate alert metrics for epidemics using
school absenteeism data. This package will allow users to simulate
school absenteeism, epidemics, and spread of people within a region
using population proportions from census data. Using these simulated
data, an optimal alert metric can be determined.

## Installation:

You can install the development version of ATQ from
[Github](https://github.com/vjoshy/ATQ_Surveillance_Package)

``` r
install.packages("devtools")
install_github("https://github.com/vjoshy/ATQ_Surveillance_Package.git")
```

## Usage

ATQ contains eight funcions which are to be used in sequentially in the
following order:  

1.  catchment_sim()
2.  elementary_pop()
3.  subpop_children()
4.  subpop_noChildren()
5.  simulate_households()
6.  simepi()
7.  model_data()
8.  eval_metrics()

``` r
library(ATQ)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
