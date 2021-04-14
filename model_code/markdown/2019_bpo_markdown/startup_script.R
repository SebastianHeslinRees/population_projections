bpo_name <- "lewisham"

#~20 to run 3 projections
housingledmodel::run_bpo_config_Jobscript(bpo_name)

#takes ~2 mins to run a full borough
source('model_code/markdown/2019_bpo_markdown/functions/run_bpo_markdown.R')
run_bpo_markdown(projection_name = bpo_name)

#upload
housingledmodel::upload_bpo_to_datastore(bpo_name,
                                         variants = c("scenario_1",
                                                      "scenario_2",
                                                      "scenario_3"))

source('model_code/markdown/2019_bpo_markdown/functions/upload_markdown.R')
bpo_slug <- upload_markdown(bpo_name)

browseURL(paste0("https://data.london.gov.uk/dataset/",bpo_slug))
