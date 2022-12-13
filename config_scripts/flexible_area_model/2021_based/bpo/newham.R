#devtools::install('model_code/flexibleareamodel', upgrade = FALSE)
library(flexibleareamodel); library(job)

bpo_name <- "newham"

#Sys.sleep(20*60)

#for(v in c("5 year constrained","10 year constrained","10 year unconstrained")){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2012:2038,
                                   variant = v)
  }, title = paste(bpo_name, v, sep = " - ")
  )
#}


# devtools::load_all('model_code/flexibleareamodel')
# run_bpo_projection(bpo_name = "newham",
#                    trajectory_range = 2012:2038,
#                    variant = "TEST 3",
#                    projection_range = 2022:2041
#                    )
