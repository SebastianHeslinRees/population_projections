library(flexibleareamodel); library(job)

bpo_name <- "brent"
variants <- "TEST"

for(v in variants){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2023:2041,
                                   variant = "TEST")
  }, title = paste(bpo_name, v, sep = " - ")
  )
}
