# GLA Population Models

The GLA's demographic models are modular, and each model combines different methods to construct its components of change - births, deaths and (most complex) migration.

The `popmodules` package contains functions for all of these, plus code that's shared between them, functions to validate population data frames, and miscellaneous functions for data wrangling.

The `trendmodel` and `flexibleareamodel` packages contain functions which leverage `popmodules` functions to produce population projections.


## Requirements

### Java
Both the 64-bit and 32-bit versions of Java must be installed. Although the model uses 64-bit R and RStudio, in order to be able to build packages both 
versions of Java are required.

### RTools
The RTools toolchain bundle must be installed before model installation is attempted. RTools contains elements which are required by some of the package dependencies in the models.

### R packages & renv
All R packages for the model are managed by `renv`. The initialize script begins with a call to `renv::restore()`. This will ensure that all necessary packages (and the correct versions) are downloaded and installed.


#### renv commands
- renv::restore() installs all the packages from the renv lock file.  You need to do this every time the lock file has changed.  This will install the packages listed in renv.lock to your renv cache and the project library (most packages are installed into your project cache and the project library folder contains symlinks to the relevant packages in the cache)

- the cache location should default to "C:/Users/[yourname]/AppData/Local/renv/cache".  It can be changed if you would like (see https://rstudio.github.io/renv/articles/renv.html)

- new packages that we need can be installed using renv::install("package", type = "binary").  To update the renv lock file with these new packages use renv::snapshot(). (type = "binary" is to avoid weird issues with testing on a 32 bit system and DLL files when installing our population packages)

- to install a specific version do e.g. renv::install("dplyr@0.8.5")

- to install from github use the github repo name e.g. renv::install("smbache/loggr")

notes:
- The Sys.setenv(JAVA_HOME="") in the .Rprofile file is a fix to something to do with Java so that devtools::install works properly. 
- renv has been set to ignore housingledmodel, popmodules, smallareamodel, flexibleareamodel, trendmodel when creating the snapshot.
This is because having them in the lockfile will cause renv::restore() to fail as it doesn't know where to look for them.


### Renviron
The BPO process uses the `ldndatar` package to push projection outputs directly to the London Datastore. In order to install the housing-led model a Github Authentication Token must be saved in an `.Renviron` file at the `population_projections` project root. The `.Reviron` should also contain the LDS API key. The file should look like this:\
lds_api_key="xxx"\
GITHUB_PAT="xxx"


## Model development

When adding new features or editing existing code the following commands should be used to test and check code before it is pushed for review:\
`devtools::check("model_code/popmodules")`\
`devtools::test("model_code/popmodules")`\
`codetools::checkUsagePackage("popmodules", suppressUndefined=TRUE)`


## Model Installation

To install, download the `population_projections` repository from the GLA GitHub at https://github.com/Greater-London-Authority/population_projections/.

Open the RStudio project within the repository root (`population_projections.Rproj`).

Run the script `input_data_scripts/initialize.R`. This will install all necessary packages from CRAN as well as the packages which comprise the GLA models suite. It will also create the data needed to run the trend and housing-led models. Raw model inputs are taken from the Q: drive and therefore the models can only be installed on machines with access to that drive.

## Legacy models
The `housing-led` (borough level) and `small area models` were used up to the 2019-based round of projections. These models were retired following the development of the `flexible area model`. Code for these model can be found in the `model_code/_retired` folder or in earlier branches of the repo.

## Running the models

Each model package has a `run_model()` function which takes a config list of model parameters as it's single input. Config lists for basic model runs can be found in the `config_scripts` folder. Note that due to models development (where parameters are added, removed or changed) config files from previous years often will often not work with current model implementation and will need to be amended to run.


## Project folder structure

The `population_projections` project is organised as follows:

- *config_scripts*: Contains configs for running scenarios. To create a new scenario use a recent config as a template. Models will provide an error message if the parameters passed from the config are not those expected by the model. In practice there are a number of parameters that have standard values. To avoid large config files a system using a `standard parameters` function is used. The config file is used to specify those parameters that the user wants control over, the others parameters pull default values from the standard parameters function. See a recent config file for an example in pactice.

- *documentation*: Was created as a location for project-specific documentation when the project was first created. In practice the folder has not been used to date.

- *input_data*: Inputs to all models are found in this folder and its sub-folders. The model itself should only read from this folder and never from a folder outside the project. All data needed to run a model must therefore be created and saved, or copied, to this folder.

- *input_data_scripts*: Scripts in this folder organise and create data and save it to the `input_data` folder. Scripts here do read from other locations (such as the Q or E drive). These scripts only need to be edited or added to when new data is needed for the model. The scripts include those which take external data (eg MYE) and re-format them into model inputs, those which take the outputs of other processes (eg regrosser) and prep them for model use and those which use internal model data (eg processed population and processed births to create fertility rates). At the folder root the `initialize` scripts provide a way of running all the necessary scripts in order to run the models.

- *model_code*: Contains folders for `popmopdules`, `trendmodel`, `flexibleareamodel`. In addition, there is a `markdown` folder containing code for various RMDs (note these are RMDs for publication not data analysis), an `other_scripts` folder which contains code that either cannot (eg python code) or should not (eg bpo upload code) be inside a model. Finally the `_retired` folder contains legacy models.
 
- *notebooks_and_analysis*: This is a folder in which random code, analysis, rmds, etc can be saved. It is excluded from being uploaded to the git repo and so each user has this area to save ongoing code from analysis projects and the like.

- *outputs*: Model outputs are saved into a folder structure.

- *renv*: The renv folder structure including `renv.lock` and `library`. See above.
