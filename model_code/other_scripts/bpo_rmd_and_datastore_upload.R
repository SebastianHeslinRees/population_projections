source("model_code/markdown/2020_bpo_markdown/process bpo into rmd inputs.R")
source("model_code/other_scripts/upload_2020_bpo_excel_rmd.R")

borough <- "camden"
wards <- "WD22"

#-------------------------------------------------------------------------------

bpo_rmd_2020(camel(borough), wards)
zip_rmds(borough)
upload_excels(borough)
upload_zip(borough)

#-------------------------------------------------------------------------------

e_proj <- list.dirs("outputs/flexible_area_model/bpo/", full.names = F, recursive = F)
q_proj <-  list.dirs("Q:/Teams/D&PA/Demography/Projections/population_models/outputs/flexible_area_model/bpo/", full.names = F, recursive = F)
diff <- setdiff(e_proj, q_proj)

if(length(diff)>0){
  for(d in 1:length(diff)){
    dir.create(paste0("Q:/Teams/D&PA/Demography/Projections/population_models/outputs/flexible_area_model/bpo/",diff[d]))
    file.copy(paste0("outputs/flexible_area_model/bpo/", diff[d]),
              paste0("Q:/Teams/D&PA/Demography/Projections/population_models/outputs/flexible_area_model/bpo/"),
              recursive = T)
  }
}