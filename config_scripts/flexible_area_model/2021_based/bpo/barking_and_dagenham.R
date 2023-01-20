library(flexibleareamodel); library(job)

bpo_name <- "barking_and_dagenham"

for(v in c("TEST")){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2012:2037,
                                   variant = "TEST")
  }, title = paste(bpo_name, v, sep = " - ")
  )
}
