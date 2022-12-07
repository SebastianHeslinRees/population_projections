library(flexibleareamodel); library(job)

# devtools::load_all('model_code/popmodules')
# devtools::load_all('model_code/flexibleareamodel')
# library(job)

bpo_name <- "newham"

for(v in c("TEST 1","TEST 2")){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2012:2038,
                                   variant = v)
  }, title = paste(bpo_name, v, sep = " - ")
 )
}
