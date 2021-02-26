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
All R packages for the model are managed by `renv`. The initialize script begins with a call to `renv::restore()`. This will ensure that all necessary packages (and the correct versions) are downloaded and installed.

### Renviron
The housing-led model uses the `ldndatar` package to push projection outputs directly to the London Datastore. In order to install the housing-led model a Github Authentication Token must be saved in an `.Renviron` file at the `population_projections` project root. The `.Reviron` should also contain the LDS API key. The file should look like this:\
lds_api_key="xxx"\
GITHUB_PAT="xxx"


## Model development

When adding new features or editing existing code the following commands should be used to test and check code before it is pushed for review:\
`devtools::check("pkg")`\
`devtools::test("pkg")`\
`codetools::checkUsagePackage("pkg", suppressUndefined=TRUE)`


## Model Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/.

Open the RStudio project within the repository root (`population_projections.Rproj`).

Run the script `input_data_scripts/initialize.R`. This will install all necessary packages from CRAN as well as the 4 packages which comprise the GLA models suite. It will also create the data needed to run the trend and housing-led models. Raw model inputs are taken from the Q: drive and therefore the models
can only be installed on machines with access to that drive.

Run `input_data_scripts/intitalize_small_area.R` will create the data needed to run the small area model.


## Running the models

Each model package has a `run_model()` function which takes a config list of model parameters as it's single input. Config lists for basic model runs can be found in the `config_scripts` folder.
