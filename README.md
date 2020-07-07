# GLA Population Models

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `popmodules` package contains functions for all of these, plus code that's shared between them, functions to validate population data frames, and miscellaneous things for data wrangling.

The `trendmodel`, `housingledmodel` and `smallareamodel` packages contain functions which leverage
`popmodules` functions to produce population projections.



## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the RStudio project within the repository root (`population_projections.Rproj`) and run the following scripts:

`input_data_scripts/initialize.R` Will install 4 packages which comprise the gla models suite. It will also create the data needed to run the trend and houing-led models.

`input_data_scripts/intitalize_small_area.R` will create the data needed to run the small area model.

`input_data_scripts/intitalize_covid_scenario_data.R` will create scenario-specific model inputs.



## Running the models

Each model package has a `run_model()` function which takes a config list of model parameters as it's single input. Config lists for basic model runs can be found in the `model_code/config_scripts` folder.

