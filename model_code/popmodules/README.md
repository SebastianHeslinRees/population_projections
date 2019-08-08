# popmodules

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `popmodules` package contains functions for all of these, plus code that's shared between them, functions to validate population data frames, and miscellaneous things for data wrangling.



## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the popmodules RStudio project within the repository (`model_code/popmodules/popmodules.Rproj`) and run
``` r
devtools::install()
```





## Contents

This is a large package, but functionality is grouped by function names:


### Births

Each births function takes an input population and and calculates the births at each of the input aggregation levels. It then sums over age, setting it to zero, and (if present) sex. If output sex is required, the births will be distributed accordingly (except with births_null).

The functions differ in just how they calculate the births.

*  `births_null` is a placeholder function: it takes a population data frame as input and returns the frame's aggregation levels with zero (or any arbitrary constant) deaths.

*  `births_from_popn_fert` takes a population data frame and a data frame containing fertility rates and combines them to calculate births and perform various checks, using `popn_apply_rate` and `validate_*` functions.




### Deaths

Each deaths function takes an input population and and calculates the deaths at each of the input aggregation levels.

*  `deaths_null` is a placeholder function: it returns zero (or any arbitrary constant) deaths.

*  `deaths_from_popn_mort` takes a population data frame and a data frame containing mortality rates and combines them to calculate deaths and perform various checks, using `popn_apply_rate` and `validate_*` functions.




### Age on

(TODO)


### Migration

(TODO)


### Validation

Validation functions conduct quick checks on population data frames and return the first argument silently (meaning they can fit into a pipeline with `%>%`). If they find something unusual, they will return an error or warning, depending how severe the problem is.

*  `validate_population()` runs basic checks on a population data frame. It checks that the specified aggregation levels behave as expected, making sure there are no duplicate levels, and optionally checks that all combinations of aggregation levels are present, that they match an example 'comparison' data frame, that the data contain no negative or missing values. See the function documentation for a full description.

*  `validate_join_population()` checks that one population is contained within another, and that any join is going to behave as expected. The nature of the matching can be constrained as desired: many-to-one and one-to-many matching can be turned on and off, and the first population can be a subset or can be required to be congruent with the second. See the function documentation for a full description.



### General population functions

Several of the above modules share code, and so this is split into functions with naming convention `popn_*`.

*  `popn_apply_rate()` applies a rate to a population, checking the validity of the inputs and output using `validate_*` functions. It takes two data frames as input, one with a population, the other with a rate (at the same or lower resolution) and returns the population aggregation levels and an output count.






## Examples

Let's create a dummy population for all these examples, and load the package:

```
library(popmodules)

popn <- expand.grid(year = 2000, age=20:23, gss_code=c("a","b","c"), sex=c("f","m"), count = 100)
```

### births

Create null births:

```
pop_births <- births_null(popn, colname_aggregation = c("gss_code", "sex", "age"), const = 0)

# equivalent to
pop_births <- births_null(popn)
```

Apply birth rates:

```
library(births)

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

### deaths

Create null deatths:

```
pop_deaths <- deaths_null(popn, colname_aggregation = c("gss_code", "sex", "age"), const = 0)

# equivalent to
pop_deaths <- deaths_null(popn)
```

Apply mortality rates:

```
mortality <- expand.grid(year=2000, age=20:21, gss_code=c("a","b"), sex=c("f","m"), rate = 0.5)

deaths <- popn_apply_rate(popn,
                          mortality,
                          col_aggregation = c("year", "gss_code", "sex", "age"),
                          col_count = "count",
                          col_rate = "rate",
                          col_deaths = "deaths")

# equivalent to
count <- popn_apply_rate(popn, mortality)
```

### Age on

(TODO)


### Migration

(TODO)



### Validation

## Example

Check a population dataset is valid:

```
library(magrittr)

validate_population(popn,
                    col_aggregation = c("year", "gss_code", "age", "sex"),
                    col_data = "count",
                    check_negative_values = TRUE)
```

Compare against a template population to make sure aggregation levels match
```
validate_population(popn,
                    col_aggregation = c("year", "gss_code", "age", "sex"),
                    comparison_pop = fert,
                    col_comparison = c("year", "gss_code", "age", "sex"))
```

Check that the two data frames can be joined together, then do the join, then validate:
```
popn_join <- popn %>%
  validate_join_population(fert,
                           cols_common_aggregation = c("year", "gss_code", "age", "sex")) %>%
  dplyr::left_join(fert, by = c("year", "gss_code", "age", "sex")) %>%
  validate_population(col_aggregation = c("year", "gss_code", "age", "sex"))
```


### General population functions

Apply a rate to a population:

```
deaths <- popn_apply_rate(popn,
                          mortality,
                          col_aggregation = c("year", "gss_code", "sex", "age"),
                          col_count = "count",
                          col_rate = "rate",
                          col_out = "deaths")

# this is equivalent to
count <- popn_apply_rate(popn, rate, col_out = "deaths")
```

