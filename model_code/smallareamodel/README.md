# smallareadmodel

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `smallareamodel` package contains functions for running the GLA small projection model. It utilises functions from the `popmodules` package to produce population projections which are consistent with input assumptions about future housing development, and outputs from the `housingled` model.

##TODO
## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the population_projections RStudio project within the repository (`population_projections.Rproj`) and run
``` r
devtools::install('model_code/smallareamodel')
```

It is recommended that users install all 4 GLA model packages (`trendmodel`, `housingledmodel`, `smallareamodel` and `popmodules`) at the same time. This can be done by opening the population_projections RStudio project within the repository and running
```r
source('model_scripts/install_gla_models')
install_gla_models()
```





## Contents

This package makes use of the `popmodules` package to produce population projections at small area. The model is flexible and will produce ward-level projections when passed ward-level inputs and MSOA-level projections when passed MSOA-level inputs.

The model reconciles population trends with forecast housing delivery to produce projected populations. These populations are constrained at the borough level for robustness and so that outputs from the small area model are consistent with those in the housing-led model.

###Projection parameters
Settings, input data file paths and model parameters are defined in a `config` file which collates all necessary input information into an R list object.

###Small Area control
The `small_are_control` module takes a single `config_list` parameter containing all of the necessary inofrmation to run a projection. The function validates the list and it's elements.

The control module uses the parameters passsed to it to read-in and process data ready for use in calculating the projected population. The function makes heavy use of the functionality provided in the `popmodules` package.

Processed data is passed into a loop where the `small_area_core` function calculates populations based on available housing and averaged compnent rates.

Once completed the outputs from the `samll_area_core` are wrangled and arranged and output.


###Small Area core
Populations consistent with housing delivery are calculated and components are derived either through the application of rates to a staring population (births and deaths) or by differencing (migration).

Aggregated small area populations are compared to borough-level population and scaling factors are derived. These factors are used to scale the small area populations. This ensures consistency with the outputs from the housing-led model and lends the small area model some of the greater robustness that the the housing-led model contains.

###Small Area arranging and outputting
The function `arrange_small_area_core_outputs` processes the raw model outputs from `small_area_core` into the format expected by `output_small_area_projections`. The latter then writes csv and rds output files.
