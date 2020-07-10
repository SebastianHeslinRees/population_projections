# housingledmodel

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `housingled` package contains functions for running the GLA housing-led projectionmodel. It utilises functions from the `popmodules` and `trendmodel` packages to produce population projections which are consistent with input assumtions about future housing development.

##TODO
## Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/. Then open the popmodules RStudio project within the repository (`model_code/popmodules/popmodules.Rproj`) and run
``` r
devtools::install()
```





## Contents

This package makes use of the `popmodules` and `trendmodel` packages to produce population projections. The stages of a projection are:

###Projection parameters
Settings, input data file paths and model parameters are defined in a `config` file which colates all necesarry input information into an R list object.

###Housing-led control
The `housingled_control` module takes a single `config_list` parameter containing all of the necessary inofrmation to run a projection. The function validates the list and it's elements.

The control module uses the parameters passed to it to read-in and process data ready for use in calculating the projected population. The function makes heavy use of the functionality provided in the `popmodules` package.

Processed data is passed into a loop where an initial trended population is calculated by running `trendmodel::trend_core` function.

This population is passed to the `housingled_core` function along with other parameters where a population consistent with housing delivery is calculated and compared to the trend population. Adjustments are made to the trend population and the component of change where necessary. The output of the `housingled_core` is passed back to the `housingled_control`.

Once completed the outputs from the `housingled_core` are wrangled and arranged and output.

###Housing-led core
The `housingled_core` module uses data on housing stock and delivery as well as occupancy rates to calculate a population consistent with forecast capacity. This target population is compared to the output from the `trend_core` and where necessary adjustments to the latter are made. Changes are in the form of adjustments to net migration into an area. The function output is a population based on the trended population but which is consistent with the amount of available housing. A consistent set of components is also output. 

The final population is passed back to the `housingled_control` and then comes back into the loop as the start population for the next year.

###Arrange and output
The `arrange_housingled_core_outputs` function takes the output from the `housingled_core` and wrangles them so that they are ready to be passed to `housingled_output` function.

The `ouput_housingled_projection` function writes out `rds`, `csv` and `excel` outputs as required.



##BPO process
The Borough Preferred Option projection (BPO) provide local authorities with the ability to run a housing-led projection based on their own housing development data. A series of function is necessary to enable to the processing of housing data and the bulk running of a number of projections.

`bpo_template_to_rds` converts a csv file containg development data into an rds file in the format expected by the housing-led model.

`run_borough_and_ward_projections` contains a set of 'standard' parameters (which can be overwritten by specifying in the input the input to the function). The function is designed so that a small set of configuration variables can be used to run both the hosuing-led and small area (ward) models. This function is not specific to the BPO process and can used elsewhere.

`run_bpo_projection` is a wrapper which runs `bpo_template_to_rds`, `run_borough_and_ward_projections` and other operations to create the parameters for the housing-led model

`output_bpo_projection` runs output scripts specific to the bpo projections including the creation of an excel file.

`run_and_upload_bpo` is a wrapper which runs a series of bpo projections, creates QA outputs and pushed data to the London Datastore using an API.

`upload_single_file_to_datstore` uses the datastore API to upload a completed BPO projection.
