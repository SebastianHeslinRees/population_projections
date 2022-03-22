library(flexibleareamodel); library(job)

bpo_name <- "hounslow_WD13"

for(v in c("lower","upper")){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2021:2035,
                                   wards = "WD13",
                                   variant = v)},
    title = paste(bpo_name, v, sep = " - ")
  )
}
