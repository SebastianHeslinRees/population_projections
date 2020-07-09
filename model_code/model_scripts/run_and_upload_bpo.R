bpo_name <- 'barnet'

library(dplyr)
library(ldndatar)
library(knitr)

#run bpo - 3 migration scenarios
source(paste0('model_code/config_scripts/housing_led/bpo_configs/',bpo_name,'.R'))

#run bpo profiles
source('model_code/markdown/bpo_markdown/bpo_markdown_data_and_render.R')
output_dir <- "outputs/housing_led/2018/bpo_reports"
root_dir <- rprojroot::find_root(rprojroot::is_git_root)
bpo_markdown_data_and_render(bpo_name, output_dir=output_dir, root_dir=root_dir)

setwd(paste0(root_dir,"/",output_dir,"/",bpo_name,'/'))
files2zip <- dir(full.names = TRUE)
zip(zipfile = paste0(paste0(root_dir,'/',output_dir,"/",bpo_name),'/',bpo_name,'.zip'), files = files2zip)
setwd(root_dir)

#upload to datastore
#DO NOT SAVE THIS FILE WITH AN API KEY
source("Q:/Teams/D&PA/Demography/Projections/population_models/notebooks_and_analysis/mats_api_key.txt")
api_key <- get_api_key()
  
#Contact info
authdemog <- "GLA Demography"
demogemail <-"demography@london.gov.uk"

bpo_root <- 'outputs/housing_led/2018/bpo'
zip_root <- 'outputs/housing_led/2018/bpo_reports'

#folder names
folder_list <- list.dirs(bpo_root, recursive = FALSE, full.names=FALSE)
folder_list <- folder_list[grepl(bpo_name, folder_list)]

ds_borough_name_lookup <- data.frame(full = c("City of London","Barking and Dagenham","Barnet","Bexley","Brent","Bromley",
                                              "Camden","Croydon","Ealing",
                                              "Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow",
                                              "Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea", "Kingston",
                                              "Lambeth","Lewisham","Merton","Newham","Redbridge",
                                              "Southwark","Sutton","Tower Hamlets",
                                              "Waltham Forest","Wandsworth","Westminster")) %>%
  mutate(full = tolower(gsub(" ","-",full))) %>%
  mutate(short = substr(full,1,4))

x <- data.frame(folder_path = folder_list) %>%
  mutate(excel_file = list.files(paste0(bpo_root,"/",folder_path), 'xlsx'),
         x = substr(folder_list,1,nchar(folder_list)-24),
         y = substr(x,nchar(x),nchar(x)),
         migration = case_when(y == "h" ~ "High",
                               y == "w" ~ "Low",
                               y == "m" ~ "Medium",
                               TRUE ~ "FALSE"),
         scenario = case_when(y == "h" ~ substr(x,1,nchar(x)-5),
                              y == "w" ~ substr(x,1,nchar(x)-4),
                              y == "m" ~ substr(x,1,nchar(x)-7),
                              TRUE ~ "FALSE"),
         b = substr(scenario,1,4)) %>%
  left_join(ds_borough_name_lookup, by=c("b"="short")) %>%
  select(-c(x,y,b)) %>%
  rename(borough=full) %>%
  mutate(zip_file = paste0(scenario,"/",scenario,".zip")) %>%
  mutate(bpo_title = gsub("_"," ",substr(excel_file,1,nchar(excel_file)-5)),
         desc = paste0("2018-based Borough Preferred Option projection. ",
                      migration," migration scenario. ",
                      "Uploaded ", format(Sys.Date(), '%d/%m/%Y'),'.'),
         zip_title = paste0(stringr::str_to_title(gsub("_"," ",scenario)), " BPO ward and borough profiles"))

#upload

#Pages and faq file
borough <- unique(x$borough)
borough <- gsub("_","-",borough)

bposlug <- paste0(borough, "-housing-led-projections---borough-preferred-option")

borough <- gsub("-"," ",borough)
borough <- stringr::str_to_title(borough)


rmd <- "Q:/Teams/D&PA/School Rolls Projection Service/Admin/Datastore/demography datastore/ds_bpo.Rmd"

#Page Metadata
lds_patch_dataset(slug = bposlug,
                  api_key = api_key,
                  patch = list(description = lds_markdown_to_description(rmd_file = rmd),
                               author = authdemog,
                               author_email = demogemail,
                               maintainer = authdemog,
                               maintainer_email = demogemail))

file.remove("ds_bpo.md")


#FAQ
lds_add_resource(file_path = "Q:/Teams/D&PA/Demography/Projections/Model Documentation/2018_BPO_projections_notes.pdf",
                 slug = bposlug,
                 api_key = api_key,
                 res_title = "2018 BPO projections: Notes & FAQ",
                 description = "Notes and FAQ for the 2018 BPO projections")



#Excel Files
for(i in 1:nrow(x)){
  
  bposlug <- paste0(x[i,5], "-housing-led-projections---borough-preferred-option")
  
  file <- paste0(bpo_root,"/",x[i,1],"/",x[i,2])
  
  lds_add_resource(file_path = file,
                   slug = bposlug,
                   api_key = api_key,
                   res_title = x[i,7],
                   description = x[i,8])
}

#zip files
y <- select(x, zip_file, borough, zip_title) %>% unique()
desc <- paste0("2018-based Borough Preferred Option html ward and borough profiles. Uploaded ",
               format(Sys.Date(), '%d/%m/%Y'),'.')

for(i in 1:nrow(y)){
  
  bposlug <- paste0(y[i,2], "-housing-led-projections---borough-preferred-option")
  
  lds_add_resource(file_path = paste0(zip_root,"/",y[i,1]),
                   slug = bposlug,
                   api_key = api_key,
                   res_title = y[i,3],
                   description = desc)
}
