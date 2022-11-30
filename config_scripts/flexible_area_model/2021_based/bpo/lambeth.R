#library(flexibleareamodel); library(job)

devtools::load_all('model_code/popmodules')
devtools::load_all('model_code/flexibleareamodel')
library(job)

bpo_name <- "lambeth"

# for(v in c("TEST")){
#   job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2012:2029,
                                   variant = "TEST")
#   }, title = paste(bpo_name, v, sep = " - ")
#  )
# }
