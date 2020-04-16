
source('model_code/markdown/bpo_markdown/bpo_markdown_data_and_render.R')

configs <- list.files("model_code/config_scripts/housing_led/bpo_configs/")
output <- "D:/temp/population_projections/outputs/housing_led/2018/bpo/reports"

for(i in 1:length(configs)){
  
  bpo_name <-  substr(configs[[i]],1,gregexpr(pattern ='.R',configs[[i]])[[1]][1]-1)
  bpo_markdown_data_and_render(bpo_name, output_dir=output)
  
}

folders_long <- list.dirs(output)
folders_short <-list.dirs(output, full.names = FALSE)
for(j in 2:length(folders_long)){
  
  setwd(folders_long[[j]])
  files2zip <- dir(full.names = TRUE)
  zip(zipfile = paste0(folders_long[[j]],'/',folders_short[[j]],'.zip'), files = files2zip)
  
}

setwd("D:/temp/population_projections/")
