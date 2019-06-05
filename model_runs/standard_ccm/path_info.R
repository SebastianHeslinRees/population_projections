#name of this configuration
config_name <- "simple_model"

#### mandatory paths ####

core_functions <- "model_code/models/standard_ccm/core_functions.R"

# functions
io_functions <- "model_code/models/standard_ccm/simple_input.R"
birth_functions <- "model_code/model_functions/births/simple_birth.R"
death_functions <- "model_code/model_functions/deaths/simple_death.R"
migration_functions <- "model_code/model_functions/migration/simple_migration.R"
visualisation_functions <- "model_code/models/standard_ccm/simple_visualisation.R"

# population estimates data
pop_path <- "input_data/simple_pop.csv" 

#### optional paths used by component functions ####

# output location

outDir <- "outputs/simple_model"

#call model script

source("model_code/models/standard_ccm/core.R")