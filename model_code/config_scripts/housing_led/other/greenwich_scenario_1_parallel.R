library(housingledmodel)

for(sc in c("high","med","low")){
  rstudioapi::jobRunScript(paste0('model_code/config_scripts/housing_led/bpo_configs/greenwich_scenario_1_',sc,".R"),
                           name = sc, encoding = "unknown",
                           workingDir = ".", importEnv = FALSE, exportEnv = "")
}
