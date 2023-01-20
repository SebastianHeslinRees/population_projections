library(flexibleareamodel); library(job)

bpo_name <- "lambeth"

variants <- "TEST"

for(v in variants){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2012:2029,
                                   variant = "TEST")
  }, title = paste(bpo_name, v, sep = " - ")
  )
}
