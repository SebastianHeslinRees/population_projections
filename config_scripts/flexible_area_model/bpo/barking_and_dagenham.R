library(smallareamodel2); library(job)

bpo_name <- "barking_and_dagenham_WD22"

for(v in c("lower","upper")){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2012:2037,
                                   wards = "WD22",
                                   variant = v)},
    title = paste(bpo_name, v, sep = " - ")
  )
}
