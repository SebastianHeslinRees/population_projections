# smallareamodel

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `smallareamodel` package contains functions for running the GLA small projection model. It utilises functions from the `popmodules` package to produce population projections which are consistent with input assumptions about future housing development, and outputs from the `housingled` model.


### Installation

It is recommended that users follow the instructions in the population projections readme (https://github.com/Greater-London-Authority/population_projections) to install all 4 of the GLA models packages rather than attempting to install them individually.

Should you need to re-install only the trend model there is a `popmodules` function to assist.
``` r
popmodules::install_gla_models(trend = FALSE, housing_led = FALSE, small_area = TRUE)
```

### Projection configuration
Settings, input data file paths and model parameters are defined in a `config` file which collates all necessary input information into an R list object.

### Run the small area model
The `run_small_area_model` module takes a single `config_list` parameter containing all of the necessary information to run a projection. The function validates the list and it's elements.

The function uses the parameters passed to it to read-in and process data ready for use in calculating the projected population. The function makes heavy use of the functionality provided in the `popmodules` package.

Processed data is passed into a loop where the `small_area_core` function calculates populations based on available housing and averaged component rates.

Once completed the outputs from the `samll_area_core` are wrangled and arranged ready for output.


### Small Area core
Populations consistent with housing delivery are calculated and components are derived either through the application of rates to a staring population (births and deaths) or by differencing (migration).

Aggregated small area populations are compared to borough-level population and scaling factors are derived. These factors are used to scale the small area populations. This ensures consistency with the outputs from the housing-led model and lends the small area model some of the greater robustness that the the housing-led model contains.

### Arrange and output
The `arrange_small_area_core_outputs` function takes the output from the `small_area_core` (which are stored in a list of lists) and wrangles them so that they are ready to be passed to `small_area_output` function.

The `ouput_small_area_projection` function writes out `rds`, `csv` and `excel` outputs as required.
