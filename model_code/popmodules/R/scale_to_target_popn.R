scale_to_target_popn <- function(initial_popn, target_popn) {
  
  scaled_popn <- initial_popn %>%
    group_by(gss_code, year) %>% # what if we were using a differnet geog to scale?
    mutate(total_popn = sum(value)) %>%
    as.data.frame() %>%
    left_join(target_popn, by = c("gss_code", "year")) %>%
    mutate(scaling_factor = value.y/total_popn, value = value.x*scaling_factor) %>%
    select(-value.x, -value.y, -scaling_factor, -total_popn)
  
  # check that scaled_popn sums to target_popn
  
  return(scaled_popn)
}