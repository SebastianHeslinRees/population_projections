# deaths

The deaths package provides functions to calculate deaths in a cohort component model.

`deaths_null` is a placeholder function: it takes a population data frame as input and returns the frame's aggregation levels with zero (or any arbitrary constant) deaths.

`deaths_from_popn_mort` takes a population data frame and a data frame containing mortality rates and combines them to calculate deaths and to perform a few checks. It returns a data frame with the input aggregation levels and a deaths column.

## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the deaths RStudio project within the repository (`model_code/method_modules/deaths/deaths.Rproj`) and run

``` r
devtools::install()
```


## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(deaths)

# Calculate deaths from mortality rates

popn <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), count = 100)
mortality <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate = 0.5)

deaths <- popn_apply_rate(popn,
                          mortality,
                          col_aggregation = c("year", "gss_code", "sex", "age"),
                          col_count = "count",
                          col_rate = "rate",
                          col_deaths = "deaths")

# Due to default parameter values, this is equivalent to
count <- popn_apply_rate(popn, mortality)
```

