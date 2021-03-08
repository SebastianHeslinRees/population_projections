library(housingledmodel)

bpo_name <- run_bpo_projection(bpo_name = "merton",
                               trajectory_range = 2012:2041,
                               variant = "low")
