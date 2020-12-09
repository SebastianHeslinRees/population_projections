# trendmodel

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `trend` package contains functions for running the GLA cohort component projection model. It utilises functions from the `popmodules` to produce population projections which are consistent with past trends in population change.


### Installation

It is recommended that users follow the instructions in the population projections readme (https://github.com/Greater-London-Authority/population_projections) to install all 4 of the GLA models packages rather than attempting to install them individually.

Should you need to re-install only the trend model there is a `popmodules` function to assist.
``` r
popmodules::install_gla_models(trend = TRUE, housing_led = FALSE, small_area = FALSE)
```

### Projection configuration
Settings, input data file paths and model parameters are defined in a `config` file which collates all necessary input information into an R list object.

### Run the trend model
The `run_trend_model` module takes a single `config_list` parameter containing all of the necessary information to run a projection. The function validates the list and it's elements.

The function uses the parameters passed to it to read-in and process data ready for use in calculating the projected population. The function makes heavy use of the functionality provided in the `popmodules` package.

It passes processed data into a loop which contains the `trend_core` and which produces a projected population and associated component outputs.

Once completed the population is passed to the household models to produce a household projection.

Finally, the outputs are arranged and written using the `arrange_trend_core_outputs` and `output_trend_projection` functions.

### Trend core

The core module is an implementation of the cohort component projection method. It takes starting population and component data nd rates and combines them to produce a final population. That population is returned to the core again as the starting population for the next year.

### Household models
Both DCLG 2014 and ONS 2018 household models are implemented in the `trendmodel` package. These models take rates and populations and output households. The models form part of the `run_trend_model` module and are passed the projected populations which are returned form the `trend_core`.

### Arrange and output
The `arrange_trend_core_outputs` function takes the output from the `trend_core` (which are stored in a list of lists) and wrangles them so that they are ready to be passed to `trend_output` function.

The `ouput_trend_projection` function writes out `rds`, `csv` and `excel` outputs as required.
