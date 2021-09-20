#2020 projections kick-off script

config_dir <- "config_scripts/housing_led/2020/"

rstudioapi::jobRunScript(path = paste0(config_dir, "2020_SHLAA.R"),
                         name = "shlaa",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir, "2020_LDD_Mean.R"),
                         name = "ldd",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir, "2020_London_Plan.R"),
                         name = "london plan",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

