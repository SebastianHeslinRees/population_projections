# births

The package provides a selection of functions to calculate births in a cohort component model.

Each function takes an input population and outputs a data frame at the population's aggregation levels, with an added births column, and with age set to zero. The functions differ in just how they calculate the births.

### births_null
provides a 'null' births module - i.e. a function `births_null()` that returns zero births. Its purpose is twofold:  
*  first, as a simplest possible births function with no data dependencies to use when testing model frameworks
*  second, as a template for building more complex births functions - biuld on its simple input and output validation functions and testthat functionality

The function returns a dataset of births ready to be joined to the aged-on parent population. An option lets you set births to a constant non-zero number amount per aggregation level.


## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the births project within the repository (`model_code/method_modules/births/births.Rproj`) and run

```
devtools::install()
```

## Example

Create null births for an example population:

``` r
library(births)
pop <- expand.grid( age=20:23, gss_code=c("a","b","c"), sex=c("f","m"), count = 100)

pop_births <- births_null(pop, colname_aggregation = c("gss_code", "sex"), const = 0)

# equivalent to
pop_births <- births_null(pop)
```

