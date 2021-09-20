#2020 projections kick-off script

config_dir <- "config_scripts/trend/2020/"

#Four main projections
rstudioapi::jobRunScript(path = paste0(config_dir, "trend_2020_CC.R"),
                         name = "CC",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir, "trend_2020_CH.R"),
                         name = "CH",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir, "trend_2020_HC.R"),
                         name = "HC",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir,"trend_2020_LC.R"),
                         name = "LC",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

#5 Supplementary
rstudioapi::jobRunScript(path = paste0(config_dir,"trend_2020_LL.R"),
                         name = "LL",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir,"trend_2020_LH.R"),
                         name = "LH",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir,"trend_2020_HH.R"),
                         name = "HH",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir,"trend_2020_HL.R"),
                         name = "HL",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")

rstudioapi::jobRunScript(path = paste0(config_dir,"trend_2020_CL.R"),
                         name = "CL",
                         encoding = "unknown",
                         workingDir = getwd(),
                         importEnv = TRUE,
                         exportEnv = "")