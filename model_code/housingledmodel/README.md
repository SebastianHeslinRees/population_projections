# housingledmodel

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `housingled` package contains functions for running the GLA housing-led projection model. It utilises functions from the `popmodules` and `trendmodel` packages to produce population projections which are consistent with input assumptions about future housing development.


### Installation

It is recommended that users follow the instructions in the population projections readme (https://github.com/Greater-London-Authority/population_projections) to install all 4 of the GLA models packages rather than attempting to install them individually.

Should you need to re-install only the housing-led model there is a `popmodules` function to assist.
``` r
popmodules::install_gla_models(trend = FALSE, housing_led = TRUE, small_area = FALSE)
```

### Projection configuration
Settings, input data file paths and model parameters are defined in a `config` file which collates all necessary input information into an R list object.

### Run the housing-led model
The `run_housing_led_model` module takes a single `config_list` parameter containing all of the necessary information to run a projection. The function validates the list and it's elements.

The `run_housing_led_model` function uses the parameters passed to it to read-in and process data ready for use in calculating the projected population. The function makes heavy use of the functionality provided in the `popmodules` package.

Processed data is passed into a loop where an initial trended population is calculated by running the `trendmodel::trend_core` function.

This population is passed to the `housing_led_core` function along with other model parameters.

### Housing-led core
The `housing_led_core` module uses data on housing stock and delivery as well as occupancy rates to calculate a population consistent with forecast capacity.

This target population is compared to the output from the `trend_core` and where necessary adjustments to the latter are made. Changes are in the form of adjustments to net migration into an area.

The function output is a population and set of components for a given year. The final population is returned to the `run_housing_led_model` module which passes it back into the next year of the projection as the starting population.

### Arrange and output
The `arrange_housingled_core_outputs` function takes the output from the `housingled_core` (which are stored in a list of lists) and wrangles them so that they are ready to be passed to `housingled_output` function.

The `ouput_housingled_projection` function writes out `rds`, `csv` and `excel` outputs as required.


### BPO process
The Borough Preferred Option projections (BPO) provide local authorities with the ability to run a housing-led projection based on their own housing development data. A series of function is necessary to enable to the processing of housing data and the bulk running of a number of projections.

`bpo_template_to_rds` converts a csv file containing development data into an rds file in the format expected by the housing-led model.

`run_borough_and_ward_projections` contains a set of 'standard' parameters (which can be overwritten by specifying them in the function parameters). The function is designed so that a small set of configuration variables can be used to run both the housing-led and small area (ward) models. This function is not specific to the BPO process and can used elsewhere.

`run_bpo_projection` is a wrapper which runs `bpo_template_to_rds`, `run_borough_and_ward_projections` and other operations to create the parameters for the housing-led model

`output_bpo_projection` runs output scripts specific to the bpo projections including the creation of an excel file.

`upload_bpo_to_datstore` uses the datastore API to upload a completed set of BPO projections.
