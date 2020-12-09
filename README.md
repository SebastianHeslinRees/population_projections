# GLA Population Models

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `popmodules` package contains functions for all of these, plus code that's shared between them, functions to validate population data frames, and miscellaneous functions for data wrangling.

The `trendmodel`, `housingledmodel` and `smallareamodel` packages contain functions which leverage `popmodules` functions to produce population projections.


## Requirements

### Java
Both the 64-bit and 32-bit versions of Java must be installed. Although the model uses 64-bit R and RStudio, in order to be able to build packages both 
versions of Java are required.

### RTools
The RTools toolchain bundle must be installed before model installation is attempted. RTools contains elements which are required by some of the package dependencies in the models.

### R packages
All R packages for the model are managed by `revn`. The initialize script begins with a call to `renv::restore()`. This will ensure that all necessary packages (and the correct versions) are downloaded and installed.

## Model Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/.

Open the RStudio project within the repository root (`population_projections.Rproj`).

Run the script `input_data_scripts/initialize.R`. This will install all necessary packages from CRAN as well as the 4 packages which comprise the GLA models suite. It will also create the data needed to run the trend and housing-led models. Raw model inputs are taken from the Q: drive and therefore the models
can only be installed on machines with access to that drive.

Run `input_data_scripts/intitalize_small_area.R` will create the data needed to run the small area model.


## Running the models

Each model package has a `run_model()` function which takes a config list of model parameters as it's single input. Config lists for basic model runs can be found in the `model_code/config_scripts` folder.

