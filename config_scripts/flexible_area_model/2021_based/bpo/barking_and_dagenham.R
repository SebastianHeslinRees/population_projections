library(flexibleareamodel); library(job)

bpo_name <- "barking_and_dagenham"
variants <- c("5-year constrained","10-year constrained")

for(v in variants){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2012:2037,
                                   variant = v)
  }, title = paste(bpo_name, v, sep = " - ")
  )
}
