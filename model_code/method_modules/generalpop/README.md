# generalpop

The GLA's demographic models are modular, and work on tidy data frames. Since code is reused between modules, the generalpop package exists to house, standardise and test frequently-used functions.

`popn_apply_rate()` applies a rate to a population, checking the validity of the inputs and output. It takes a two data frames as input, one with a population, the other with a rate (at the same or lower resolution) and returns the population aggregation levels and an output count.

## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the generalpop RStudio project within the repository (`model_code/method_modules/generalpop/generalpop.Rproj`) and run
``` r
devtools::install()
```

## Example

Apply a rate to a population:

``` r
library(generalpop)

popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), count = 100)
rate <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate = 0.5)

count <- popn_apply_rate(popn,
                         rate,
                         col_aggregation = c("year", "gss_code", "sex", "age"),
                         col_count = "count",
                         col_rate = "rate",
                         col_out = "value")

# Due to default parameter values, this is equivalent to
count <- popn_apply_rate(popn, rate)
```

