library(flexibleareamodel); library(job)

# bpo_name <- "barnet_WD13"
# 
# for(v in c("lower","upper")){
#   job({
#     bpo_name <- run_bpo_projection(bpo_name = bpo_name,
#                                    trajectory_range = 2012:2035,
#                                    wards = "WD13",
#                                    variant = v)},
#     title = paste(bpo_name, v, sep = " - ")
#   )
# }

bpo_name <- "barnet_v2_WD13"

for(v in c("lower","upper")){
  job({
    bpo_name <- run_bpo_projection(bpo_name = bpo_name,
                                   trajectory_range = 2022:2036,
                                   wards = "WD13",
                                   variant = v)},
    title = paste(bpo_name, v, sep = " - ")
  )
}
