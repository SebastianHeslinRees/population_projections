library(housingledmodel)

bpo_name <- run_bpo_projection(bpo_name = "lewisham_local_plan",
                               trajectory_range = 2020:2035,
                               variant = scenario)
