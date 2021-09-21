#2020 projections kick-off script

config_dir <- "config_scripts/housing_led/2020/"

rstudioapi::jobRunScript(path = paste0(config_dir, "2020_Identified_Capacity.R"),
                         name = "identified capacity",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir, "2020_Past_Delivery.R"),
                         name = "past delivery",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir, "2020_Housing_Targets.R"),
                         name = "housing targets",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

