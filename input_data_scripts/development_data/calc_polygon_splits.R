#Where LDD permission polygons are split cross LSOA polygons
#This script determines what % of the LDD polygon is in what LSOA
#Output is a csv lookup file between permission & LSOA with a %
#Original code from Libby Rogers, 2019-12-18

# Be careful with igraph - has lots of masking issues
library(igraph)
library(tidyverse)
library(sf)
library(smoothr)
library(lwgeom)

#Its probably worth re-starting R
rm(list=ls())
gc()

################################################################################
##################### Determine final permission polygons ######################

#Input data - Check this doesn't need updating
perm_poly_file <- "N:/LDD/GIS download/ldd_nightly_download.gdb"
ldd_unit_flow_file <- "N:/LDD/Unit flow analysis/output_data/IMA/2020_06/0_ldd_development_unit_flow.Rda"



# Read LDD development flow data
load(ldd_unit_flow_file)

#Group any duplicates
ldd_development_unit_flow <- ldd_development_unit_flow %>%
  group_by(permission_id, dev_id, ssd_by, borough_ref, lsoa11cd, status_line_flow, ssd,
           date_work_start, date_work_comp, demolition) %>% 
  summarise(unit_line_flow = sum(unit_line_flow),
            .groups = 'drop_last') %>% 
  data.frame()

#Read LDD polygons
polys_in <- st_read(perm_poly_file, layer = "ldd_polygons_clean",
                    stringsAsFactors = FALSE)

# Initial poly data cleaning
# Remove polygons which supersede earlier permissions
# Should give us a better idea of where units were/will be actually built
polys <- polys_in %>%
  select(permission_id) %>%
  filter(!is.na(st_dimension(., NA_if_empty = TRUE))) %>%
  sf::st_make_valid(.) %>%
  rename(geometry = SHAPE)
st_geometry(polys) <- "geometry"

# Get the outcomes for permissions with polygons associated
# Do completions and demolitions separately
poly_comps <- ldd_development_unit_flow %>% 
  filter(status_line_flow == "comp",
         demolition == FALSE)

poly_demos <- ldd_development_unit_flow %>% 
  filter(status_line_flow %in% c("comp","start"),
         demolition == TRUE)

poly_others <- setdiff(ldd_development_unit_flow, rbind(poly_comps, poly_demos)) %>% 
  mutate(outcome = ifelse(status_line_flow == "ssd-rep", "ssd", "lap"))
  
poly_outcomes <- rbind(poly_comps, poly_demos) %>% 
  mutate(outcome = "cont") %>% 
  rbind(poly_others) %>% 
  group_by(dev_id, permission_id, outcome, demolition) %>%
  summarise(units = sum(unit_line_flow), .groups = 'drop_last') %>%
  as.data.frame() %>%
  spread(outcome, units, 0) %>%
  mutate(cont_prop = cont / (cont + lap + ssd)) %>%
  inner_join(polys, ., by = "permission_id")

rm(poly_comps, poly_demos, poly_others)

# Get separate networks for demolitions and new units
dev_nodes <- ldd_development_unit_flow %>%
  pull(permission_id) %>%
  unique()

demo_edges <- ldd_development_unit_flow %>%
  filter(demolition == TRUE) %>%
  select(permission_id, ssd_by) %>%
  # Get permission level superseding
  mutate(ssd_by = gsub("_[0-9]+$", "", ssd_by)) %>%
  drop_na() %>%
  distinct()

demo_graph <- igraph::graph_from_data_frame(
  d = demo_edges, vertices = dev_nodes, directed = TRUE)

new_edges <- ldd_development_unit_flow %>%
  filter(demolition == FALSE) %>%
  select(permission_id, ssd_by) %>%
  mutate(ssd_by = gsub("_[0-9]+$", "", ssd_by)) %>%
  drop_na() %>%
  distinct()

new_graph <- igraph::graph_from_data_frame(
  d = new_edges, vertices = dev_nodes, directed = TRUE)

# Only need to adjust polygons if
# They have some superseded and some continuing units
# Others are either removed entirely, or whole polygon can be used
polys_to_adjust <- poly_outcomes %>%
  filter(ssd != 0 & cont != 0)

new_polys <- poly_outcomes

# Loop over the polygons we need to adjust
message(paste(nrow(polys_to_adjust), "polygons to process"))

for (row in 1:nrow(polys_to_adjust)) {
  perm <- polys_to_adjust[[row, "permission_id"]]
  demo <- polys_to_adjust[[row, "demolition"]]
  cont_prop <- polys_to_adjust[[row, "cont_prop"]]
  
  message(paste0(row,": ",perm))
  
  orig_poly <- polys_to_adjust %>%
    filter(permission_id == perm) %>%
    filter(demolition == demo)
  
  # Extract descendants
  if (demo == TRUE) {
    ssders <- distances(demo_graph, perm, mode = "out") %>%
      as.data.frame() %>%
      gather() %>%
      filter(value != Inf) %>%
      filter(key != perm) %>%
      pull(key)
  } else {
    ssders <- distances(new_graph, perm, mode = "out") %>%
      as.data.frame() %>%
      gather() %>%
      filter(value != Inf) %>%
      filter(key != perm) %>%
      filter(key %in% polys$permission_id) %>%
      pull(key)
  }
  
  if (length(ssders) != 0) {
    # Merge descendents into one geom
    ssders_polys <- polys %>%
      filter(permission_id %in% ssders) %>%
      select(geometry)
    
    # Take it away from the original polygon
    remain_ssded <- orig_poly %>%
      st_difference(., ssders_polys) %>%
      # Drop small artifacts
      smoothr::drop_crumbs(., units::set_units(100, m^2))
    
    # Compare % of units left with % of area left
    remain_area_prop <- cont_prop / (
      as.numeric(st_area(remain_ssded))
      / as.numeric(st_area(orig_poly)))
    
    remain_area_prop <- ifelse(is_empty(remain_area_prop), 10,
                               remain_area_prop)
    
    # Only want to replace if there's a realistic area left
    # Should be roughly 1ish - anything much larger than this
    # and we've probably removed more than is realistic
    if (remain_area_prop < 2.1) {
      message("changed")
      new_polys <- new_polys %>%
        filter(!(permission_id == perm & demolition == demo)) %>%
        rbind(., remain_ssded)
    }
  }
}

st_crs(new_polys) <- 27700

################################################################################
######################## Remove Blue and Green cover ###########################

# # Ignore for now
# # This is water and large parks
#
# blue_and_green <- st_read("N:/LDD/Unit flow analysis/input_data/blue_green_cover.shp", crs = 27700)
# blue_and_green <- blue_and_green %>%
#   st_cast(., "POLYGON")
# 
# # Need to simplify this polygon, takes ages!
# no_or_all_blue_green <- new_polys %>%
#   filter(lengths(st_intersects(., blue_and_green)) == 0 |
#            lengths(st_within(., blue_and_green)) > 0)
# 
# some_blue_green <- new_polys %>%
#   filter(!(permission_id %in% no_or_all_blue_green$permission_id))
# 
# blue_and_green_meet <- blue_and_green %>%
#   filter(lengths(st_intersects(., some_blue_green)) > 0) %>%
#   st_union()
# 
# 
# remove_blue_green <- some_blue_green %>%
#   st_difference(., blue_and_green_meet) %>%
#   st_as_sf()
# 
# blue_green_removed <- rbind(
#   remove_blue_green,
#   no_or_all_blue_green)
# 
# st_geometry(blue_green_removed) <- blue_green_removed$geometry

################################################################################
############################ Split polygons by LSOA ############################


lsoa_file <- "W:/GISDataMapInfo/BaseMapping/Boundaries/StatisticalBoundaries/Census_2011/SuperOutputAreas/London/Lower/ESRI/LSOA_2011_London.shp"

lsoa <- st_read(lsoa_file, stringsAsFactors = FALSE)
lsoa <- lsoa %>%
  select(LSOA11CD, LSOA11NM) %>%
  setNames(., tolower(names(.)))

st_crs(lsoa) <- 27700

# If removed blue/green replace new_polys with blue_green_removed
area_split <- new_polys %>%
  mutate(area = as.numeric(st_area(.))) %>%
  st_intersection(., lsoa) %>%
  mutate(lsoa_area = as.numeric(st_area(.))) %>%
  mutate(area_prop = lsoa_area / area) %>%
  mutate(dev_id = as.integer(dev_id))

#There needs to be a break here
area_split <- area_split %>% 
  data.frame() %>% 
  select(permission_id, dev_id, demolition, lsoa11cd, lsoa11nm, area_prop)

dir.create("input_data/housing_led_model", showWarnings = FALSE)
saveRDS(area_split, "input_data/housing_led_model/lsoa_polygon_splits.rds")
