# trendmodel

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `trend` package contains functions for running the GLA cohort component model. It utilises functions from the `popmodules` package to read in, wrangle and process data to calculate rates and flows based on past trends. It then applies these to a starting population in an incremental loop to create a population projection.


###TODO
## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the popmodules RStudio project within the repository (`model_code/popmodules/popmodules.Rproj`) and run
``` r
devtools::install()
```





## Contents

This package makes use of the `popmodules` package to produce population projections. The stages of a projection are:

###Projection parameters
Settings, input data file paths and model parameters are defined in a `config` file which collates all necessary input information into an R list object.

###Trend control
The `trend_control` module takes a single `config_list` parameter containing all of the necessary information to run a projection. The function validates the list and its elements.

The control module uses the parameters passsed to it to read-in and process data ready for use in calculating the projected population. The function makes heavy use of the functionality provided in the `popmodules` package.

Processed data is passed into a loop where the `trend_core` function calculates populations in year increments using the output of one year as the input to the next (see below).

Once completed the outputs from the `trend_core` are wrangled and arranged.

The `trend_control` also passed projected populations into `household` models (see below).

Finally, the `trend_control` manages the outputs of the model (see below).

###Trend core
The `trend_core` module takes rates, flows and populations for a single year and combines them using a standard cohort component approach to procude an output population. The module makes heavy use of the `popmodules` package.

The core takes a starting population and ages it on 1 year. Births, deaths and migration are calcualted by applying fertility, mortality and migration rates to the population.

The components are added/subtracted as necessary to the start population to reach the final population. The final population is passed back to the `trend_control` and then comes back into the `trend_core` as the start population for the next year.

###Household models
Both DCLG 2014 and ONS 2016 household models are implemented in the `trendmodel` package. These models take rates and populations and output households. The models form part of the `trend_control` module and are passed the projected populations which are returned form the `trend_core`.

###Arrange and output
The `arrange_trend_core_outputs` function takes the output from the `trend_core` and wrangles them so that they are ready to be passed to `trend_output` function. This function sits with the `trend_control` function.

The `trend_output` function writes out `rds`, `csv` and `excel` outputs as required.
