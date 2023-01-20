library(flexibleareamodel); library(job)

bpo_name <- "hounslow"
variants <- "TEST"

for(v in variants){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2021:2035,
                                   variant = "TEST")
  }, title = paste(bpo_name, v, sep = " - ")
  )
}
