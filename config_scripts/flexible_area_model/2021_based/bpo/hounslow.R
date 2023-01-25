library(flexibleareamodel); library(job)

bpo_name <- "hounslow"
variants <- c("5-year constrained","10-year constrained")

for(v in variants){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2021:2035,
                                   variant = v)
  }, title = paste(bpo_name, v, sep = " - ")
  )
}

#Don't run until the above are completed
source("model_code/markdown/2021_bpo_markdown/process bpo into rmd.R")
bpo_rmd_2021(bpo_name)
