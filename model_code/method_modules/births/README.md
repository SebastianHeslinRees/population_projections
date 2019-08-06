# births

The package provides a selection of functions to calculate births in a cohort component model.

Each function takes an input population and outputs a data frame at the population's aggregation levels, with an added births column, and with age set to zero and sex split according to options set within the function (ecept with births_null). The functions differ in just how they calculate the births.

`births_null` is a placeholder function: it takes a population data frame as input and returns the frame's aggregation levels with zero (or any arbitrary constant) deaths.

`births_from_popn_fert` takes a population data frame and a data frame containing mortality rates and combines them to calculate deaths and to perform a few checks. It returns a data frame with the input aggregation levels and a deaths column.



## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the births project within the repository (`model_code/method_modules/births/births.Rproj`) and run

```
devtools::install()
```

## Example

Create null births for an example population:

``` r
library(births)
pop <- expand.grid(year = 2000, age=20:23, gss_code=c("a","b","c"), sex=c("f","m"), count = 100)

pop_births <- births_null(pop, colname_aggregation = c("gss_code", "sex"), const = 0)

# equivalent to
pop_births <- births_null(pop)
```

Apply birth rates to an example population:

``` r
library(births)
popn <- expand.grid(year = 2000, age=20:23, gss_code=c("a","b","c"), sex=c("f","m"), count = 100)
fert <- expand.grid(year = 2000, age=20:23, gss_code=c("a","b","c"), rate = 0.01)

pop_births <- births_from_popn_fert(popn,
                                    fert,
                                    colname_aggregation = c("year", gss_code", "age", "sex"),
                                    col_age = "age",
                                    col_sex = "sex",
                                    col_count = "count",
                                    col_rate = "rate",
                                    col_births = "births",
                                    birthratio_m2f = 1.05)
# equivalent to
pop_births <- births_from_popn_fert(popn, fert)
```
