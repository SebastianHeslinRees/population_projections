# validatepop

validatepop provides a couple of functions to fit into demographic work in R. They conduct quick checks on the input(s) and return the first argument silently (meaning they can fit into a pipeline with `%>%`).

`validate_population()` runs basic checks on a data frame. It checks that the specified aggregation levels behave as expected, making sure there are no duplicate levels, and optionally checks that all combinations of aggregation levels are present, that they match an example 'comparison' data frame, that the data contain no negative or missing values. See the function documentation for a full description.

`validate_join_population()` checks that one population is contained within another, and that any join is going to behave as expected. The nature of the matching can be constrained as desired: many-to-one and one-to-many matching can be turnned on and off, and the first population can be a subset or can be required to be congruent with the second. See the function documentation for a full description.

## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the validatepop project within the repository (`model_code/validatepop/validatepop.Rproj`) and run

```
devtools::install()
```


## Example

Check a population dataset is valid:

```r
library(validatepop)
library(magrittr)

pop <- expand.grid(age = 0:10, geog = c("a","b","c","d"), sex = c("m","f"), count = 100)

validate_population(pop,
                    col_aggregation = c("age", "geog", "sex"),
                    col_data = "count",
                    check_negative_values = TRUE)
```

Compare against a template population to make sure aggregation levels match
```r
pop2 <- dplyr::mutate(pop, class = "a") %>%
  dplyr::rename(gss = geog, count2 = count)

validate_population(pop,
                    col_aggregation = c("age", "geog", "sex"),
                    comparison_pop = pop2,
                    col_comparison = c("age", "geog" = "gss", "sex"))
```

Check that the two data frames can be joined together, then do the join, then validate:
```r
pop_join <- pop %>%
  validate_join_population(pop2,
                           cols_common_aggregation = c("age","geog"="gss", "sex")) %>%
  dplyr::left_join(pop2, by = c("age","geog"="gss", "sex")) %>%
  validate_population(col_aggregation = c("age","geog", "sex"))
```
