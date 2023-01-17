devtools::load_all("Q:/Teams/D&PA/Demography/demogtools")
source("model_code/markdown/2021_projections/demog_plotly.R")
library(dplyr)
library(data.table)
library(leaflet)
library(rgdal)
library(tidyr)

f <- 1
root <- "outputs/trend/2021/"
root_2020 <- "outputs/trend/2020/"
flex_root <- "outputs/flexible_area_model/2021_based/"
trend_factor_levels <- c("5-year trend", "10-year trend","15-year trend")

#
projections_2020 <- list('Central Upper' = "2020_CC_central_upper_21-09-21_1259",
                         'Central Lower' = "2020_CH_central_lower_21-09-21_1259") %>% 
  lapply(function(x) paste0(root_2020,x,"/"))

#
projections_2021<- list('Central' = "2021_10yr_23-01-17_1210",
                        'Short' = "2021_5yr_23-01-17_1210",
                        'Long' = "2021_15yr_23-01-17_1210") %>% 
  lapply(function(x) paste0(root,x,"/"))

#
components <- c("population","births","deaths","dom_in","dom_out","dom_net",
                "int_in","int_out","int_net","total_net","natural_change")

#
households <- c("ons_stage_2_households","dclg_stage_2_households")

#
housing_led <- list('Identified Capacity' = "2021_Identified_Capacity_10yr",
                    'Past Delivery' = "2021_Past_Delivery_10yr",
                    'Housing Targets' = "2021_Housing_Targets_10yr") %>%
  lapply(function(x) paste0(flex_root,x,"/"))

#-------------------------------------------------------------------------------

re_factor_variant <- function(x){
  x %>% 
    filter(!variant %in% c("Central Lower",
                           "Central Upper")) %>% 
    mutate(variant = case_when(variant == "Central" ~ "10-year trend",
                               variant == "Short" ~ "5-year trend",
                               variant == "Long" ~ "15-year trend",
                               TRUE ~ "TRUE")) %>% 
    mutate(variant = factor(variant, levels = c("5-year trend", "10-year trend", "15-year trend")))
}

#-------------------------------------------------------------------------------

data_list <- list()
charts <- list()

for(i in components){
  print(i)
  data_list[[i]] <- read_multiple_projections(projections_2021, i) %>% 
    re_factor_variant()
}

print('age structure')
data_list[['sya']] <- read_multiple_projections(projections_2021,
                                                "population",
                                                col_aggregation = c("year","gss_code","age")) %>% 
  re_factor_variant()



print('households')
projections <- lapply(projections_2021, function(x) paste0(x,"households/"))

for(i in households){
  print(i)
  data_list[[i]] <- read_multiple_projections(projections,i,age_filter = NULL) %>% 
    re_factor_variant()
  
}

data_list[['popn_2020']] <- read_multiple_projections(projections_2020, "population") %>% 
  mutate(projection = "2020-based")

#-------------------------------------------------------------------------------

#5-year horizon Chart
chart_title <- "Projected Population (2021-2026), London"
axis_title <- "population"
f <- f+1

charts[['short']] <- data_list[['population']] %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% 2021:2026) %>% 
  plot_ly(x=~year, y=~popn, color=~variant,
          colors = c("#6da7de",
                     "#9e0059",
                     "#dee000"),
          type = "scatter",
          mode = "lines+markers", 
          line = list(shape = "spline"),
          text = ~variant,
          hovertemplate = paste0("<b>%{x}</b><br>","%{text}: %{y:,.0f}","<extra></extra>")) %>% 
  layout(title = list(text = paste0("<b>Figure ",f,": ",chart_title,"</b>"),
                      xanchor = "left",
                      x = 0,
                      font = list(size = 12)),
         xaxis = list(title = "",
                      showgrid = FALSE),
         yaxis = list(title = "population",
                      showgrid = TRUE),
         legend = list(orientation = "h"),
         margin = list(b=50),
         autosize = TRUE,
         annotations = list(x = 1,
                            y = -0.10, #position of text adjust as needed 
                            text = "Source: GLA 2021-based population projections", 
                            showarrow = F,
                            xref='paper',
                            yref='paper', 
                            xanchor='right',
                            yanchor='auto',
                            xshift=0,
                            yshift=0,
                            font=list(size=11, color="grey")))

#Population Chart
chart_title <- "Projected Population, London"
axis_title <- "population"
f <- f+1

charts[['popn']] <- data_list[['population']] %>% 
  filter_london(data_col = "popn") %>% 
  line_chart_plotly(chart_title,
                    "year", "popn", "variant",
                    axis_title, figure = f, zero_y = FALSE)


charts[['exec summary']] <- data_list[['population']] %>% 
  filter_london(data_col = "popn") %>% 
  line_chart_plotly(chart_title,
                    "year", "popn", "variant",
                    axis_title, figure = 1, zero_y = FALSE) 

#comparison chart
chart_title <- "Projected Population, London"
axis_title <- "population"
f <- f+1

charts[['comp']] <- data_list[['population']] %>% 
  mutate(projection = "2021-based") %>% 
  rbind(data_list[['popn_2020']]) %>% 
  mutate(x = paste(projection, variant)) %>% 
  filter_london(data_col = "popn", validate = FALSE) %>% 
  select(gss_code, year, x, popn) %>% 
  mutate(x = factor(x, levels = c('2021-based 5-year trend',
                                  '2021-based 10-year trend',
                                  '2021-based 15-year trend',
                                  '2020-based Central Lower',
                                  '2020-based Central Upper'))) %>% 
  line_chart_plotly(chart_title,
                    "year", "popn", "x",
                    axis_title, figure = f, zero_y = FALSE) 

#Age structure
chart_title <- "Projected age structure 2036, London"
axis_title <- "population"
f <- f+1

charts[['age structure']] <- data_list[['sya']] %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% c(2021,2037)) %>% 
  mutate(variant = as.character(variant)) %>% 
  mutate(variant = ifelse(year == 2021, "2021 structure", variant)) %>% 
  unique() %>% 
  mutate(value = popn) %>% 
  mutate(variant = factor(variant, levels = c("2020 structure", trend_factor_levels))) %>% 
  data.frame() %>% 
  age_chart_plotly(chart_title, colour = "variant", figure = f)

#Working age
chart_title <- "Working age population projections, London"
axis_title <- "population aged 16-64"
f <- f+1

charts[['working age']] <- data_list[['sya']] %>% 
  filter_london(data_col = "popn") %>% 
  filter(age %in% 16:64) %>%
  group_by(variant, year) %>% 
  summarise(popn = sum(popn), .groups = 'drop_last') %>% 
  data.frame() %>% 
  line_chart_plotly(chart_title,
                    "year", "popn", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#Total migration
chart_title <- "Total net migration, London"
axis_title <- "persons"
f <- f+1

charts[['total net']] <- data_list[['total_net']] %>% 
  filter_london(data_col = "total_net") %>% 
  #re_factor_variant() %>% 
  line_chart_plotly(chart_title,
                    "year", "total_net", "variant",
                    axis_title, figure = f, zero_y = FALSE)


#International migration
chart_title <- "Net international migration, London"
f <- f+1

charts[['int net']] <- data_list[['int_net']] %>% 
  filter_london(data_col = "int_net") %>% 
  #re_factor_variant() %>% 
  line_chart_plotly(chart_title,
                    "year", "int_net", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#

chart_title <- "International in migration, London"
f <- f+1

charts[['int in']] <- data_list[['int_in']] %>% 
  filter_london(data_col = "int_in") %>% 
  #re_factor_variant() %>% 
  line_chart_plotly(chart_title,
                    "year", "int_in", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#

chart_title <- "International out migration, London"
f <- f+1

charts[['int out']] <- data_list[['int_out']] %>% 
  filter_london(data_col = "int_out") %>% 
  #re_factor_variant() %>% 
  line_chart_plotly(chart_title,
                    "year", "int_out", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#Domestic migration
chart_title <- "Net domestic migration, London"
f <- f+1

charts[['dom net']] <- data_list[['dom_net']] %>% 
  filter(gss_code == "E12000007") %>% 
  #re_factor_variant() %>% 
  line_chart_plotly(chart_title,
                    "year", "dom_net", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#

chart_title <- "Domestic in migration, London"
f <- f+1

charts[['dom in']] <- data_list[['dom_in']] %>% 
  filter(gss_code == "E12000007") %>% 
  #re_factor_variant() %>% 
  line_chart_plotly(chart_title,
                    "year", "dom_in", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#

chart_title <- "Domestic out migration, London"
f <- f+1

charts[['dom out']] <- data_list[['dom_out']] %>% 
  filter(gss_code == "E12000007") %>%  
  #re_factor_variant() %>% 
  line_chart_plotly(chart_title,
                    "year", "dom_out", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#Births
chart_title <- "Total births, London"
f <- f+1
axis_title <- "deaths"

charts[['births']] <- data_list[['births']] %>% 
  filter_london(data_col = "births") %>% 
  line_chart_plotly(chart_title,
                    "year", "births", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#Deaths
chart_title <- "Total deaths, London"
f <- f+1
axis_title <- "deaths"

charts[['deaths']] <- data_list[['deaths']] %>% 
  filter_london(data_col = "deaths") %>% 
  line_chart_plotly(chart_title,
                    "year", "deaths", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#Natural change
chart_title <- "Natural change, London"
f <- f+1
axis_title <- "population"

charts[['natural change']] <- data_list[['deaths']] %>% 
  mutate(deaths = deaths*-1) %>% 
  rename(popn = deaths) %>% 
  rbind(data_list[['births']] %>% rename(popn = births)) %>% 
  filter_london(data_col = "popn") %>% 
  line_chart_plotly(chart_title,
                    "year", "popn", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#Households ONS
chart_title <- "Total households, London (ONS model)"
f <- f+1
axis_title <- "households"

charts[['ons hh']] <- data_list[['ons_stage_2_households']] %>% 
  filter_london(data_col = "households") %>% 
  filter(year >= 2011) %>% 
  line_chart_plotly(chart_title,
                    "year", "households", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#Households DCLG
chart_title <- "Total households, London (DCLG model)"
f <- f+1
axis_title <- "households"

charts[['dclg hh']] <- data_list[['dclg_stage_2_households']] %>% 
  filter_london(data_col = "households") %>% 
  line_chart_plotly(chart_title,
                    "year", "households", "variant",
                    axis_title, figure = f, zero_y = FALSE)

charts <- lapply(charts, layout, autosize=T)

#-------------------------------------------------------------------------------

#### Housing-led

#data
hlm_factor_levels <- c("Identified Capacity","Past Delivery","Housing Targets")

housing_led_popn <- read_multiple_projections(housing_led, "population")

housing_led_sya <- read_multiple_projections(housing_led, "borough_data.population",
                                             col_aggregation = c("gss_code","year","age"))

housing_led_change <- housing_led_popn %>% 
  filter(year %in% c(2021,2041)) %>% 
  mutate(popn = ifelse(year == 2021, popn*-1, popn)) %>% 
  group_by(gss_code, variant) %>% 
  summarise(change = sum(popn), .groups = "drop_last") %>% 
  data.frame() %>% 
  rename(GSS_CODE = gss_code)

hlm_vs_trend <- data_list[['population']] %>% 
  filter_london(data_col = "popn") %>% 
  rbind(housing_led_popn %>% 
          filter_london(data_col = "popn")) %>% 
  filter(year %in% 2011:2041) %>%
  pivot_wider(names_from = variant, values_from = popn)

housing_targets_dev <- readRDS("input_data/flexible_area_model/development_data/housing_targets_WD22CD.rds") %>% 
  mutate(variant = "Housing Targets")
shlaa_dev <- readRDS("input_data/flexible_area_model/development_data/ward_savills_trajectory_WD22CD.rds") %>% 
  mutate(variant = "Identified Capacity")
past_delivery_dev <- readRDS("input_data/flexible_area_model/development_data/past_delivery_WD22CD.rds") %>% 
  mutate(variant = "Past Delivery")

WD22CD_to_LAD21CD <- readRDS("input_data/flexible_area_model/lookups/ward_2022_name_lookup.rds") %>% 
  select(gss_code, gss_code_ward)

dev_data <- rbind(housing_targets_dev, shlaa_dev, past_delivery_dev) %>% 
  left_join(WD22CD_to_LAD21CD, by = "gss_code_ward") %>% 
  select(-gss_code_ward) %>% 
  filter_london(data_col = "units", validate = FALSE) %>% 
  mutate(variant = factor(variant, levels = hlm_factor_levels))

ahs <- read_multiple_projections(housing_led, "borough_data.households_detail", age = NULL,
                                 col_aggregation = c("gss_code","year","household_population","input_households")) %>% 
  filter(year > 2010) %>% 
  group_by(year, variant) %>% 
  summarise(ahs = sum(household_population)/sum(input_households),
            .groups = 'drop_last') %>% 
  data.frame()

#-------------------------------------------------------------------------------

#charts

#-------------------------------------------------------------------------------

#development

chart_title <- "Forecast dwellings, London"
f <- f+1
axis_title <- "net additonal dwellings"

charts[['dev chart']] <- dev_data %>% 
  filter(year %in% 2012:2041) %>%
  line_chart_plotly(chart_title,
                    "year", "units", "variant",
                    axis_title, figure = f, zero_y = TRUE,
                    source = "Source: London development database, GLA 2021-based population projections")


chart_title <- "Forecast housing stock, London"
f <- f+1
axis_title <- "total dwellings"

charts[['stock']] <- dev_data %>% 
  filter(year %in% 2011:2041) %>%
  group_by(variant) %>% 
  mutate(stock = cumsum(units)) %>% 
  arrange(variant, year) %>% 
  data.frame() %>% 
  line_chart_plotly(chart_title,
                    "year", "stock", "variant",
                    axis_title, figure = f, zero_y = FALSE,
                    source = "Source: London development database, GLA 2021-based population projections")


#-------------------------------------------------------------------------------

#population
chart_title <- "Projected Population, London"
axis_title <- "population"
f <- f+1

charts[['trend-vs-hlm']] <- hlm_vs_trend %>% 
  
  #shlaa line
  plot_ly(x = ~year, y = ~`Identified Capacity`,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = "#6da7de",
                      shape = "spline"),
          showlegend = TRUE,
          name = 'Identified Capacity',
          text = 'Identified Capacity',
          hovertemplate = paste0("<b>%{x}</b><br>","%{text}: %{y:,.0f}","<extra></extra>")) %>% 
  
  #ldd line
  add_trace(x = ~year, y = ~`Past Delivery`,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color =  "#9e0059",
                        shape = "spline"),
            marker = list(color =  "#9e0059"),
            showlegend = TRUE,
            name = 'Past Delivery',
            text = 'Past Delivery') %>% 
  
  #london plan line
  add_trace(x = ~year, y = ~`Housing Targets`,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = "#dee000",
                        shape = "spline"),
            marker = list(color =  "#dee000"),
            showlegend = TRUE,
            name = 'Housing Targets',
            text = "Housing Targets") %>% 
  #Central
  add_trace(x = ~year, y = ~`10-year trend`,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = "#dee000",
                        shape = "spline"),
            marker = list(color =  "#dee000"),
            showlegend = TRUE,
            name = '10-year trend',
            text = "10-year trend") %>% 
  
  #Short
  add_trace(x = ~year, y = ~`5-year trend`, type = 'scatter', mode = 'lines',
            line = list(color = 'rgba(0,100,80,0.2)'),
            showlegend = TRUE, name = '5-year trend', text = '5-year trend') %>% 
  
  #Central Upper line and shading between the 2
  add_trace(y = ~`15-year trend`, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
            line = list(color = 'rgba(0,100,80,0.2)'),
            showlegend = FALSE, name = '15-year trend', text = '15-year trend') %>% 
  
  #layout and presentation options
  layout(title = list(text = paste0("<b>Figure ",f,": ",chart_title,"</b>"),
                      xanchor = "left",
                      x = 0,
                      font = list(size = 12)),
         xaxis = list(title = "",
                      showgrid = FALSE),
         yaxis = list(title = axis_title,
                      showgrid = TRUE),
         legend = list(orientation = "h",
                       traceorder = "reversed"),
         margin = list(b=50),
         autosize = TRUE,
         
         #3 rectangles for different periods
         shapes = list(
           list(type = "rect",
                fillcolor = "#44aad5",
                line = list(width = 0),
                opacity = 0.15,
                x0 = 2021, x1 = 2025,
                y0 = 8000000, y1 =11000000,
                layer = "below"),
           
           list(type = "rect",
                fillcolor = "#44aad5",
                line = list(width = 0),
                opacity = 0.05,
                x0 = 2025, x1 = max(hlm_vs_trend$year+1),
                y0 = 8000000, y1 =11000000,
                layer = "below")), 
         
         # labels at the top of the rectangles
         annotations = list(
           list(x = 2023,
                y = 0.93,
                text = "Short-term\nperiod",
                xref = "x",
                yref = "paper",
                xanchor = 'center',
                yanchor = 'top',
                showarrow = FALSE,
                font=list(size=11)),
           
           list(x = mean(c(2025,2041)),
                y = 0.93,
                text = "Long-term\nperiod",
                xref = "x",
                yref = "paper",
                xanchor = 'center',
                yanchor = 'top',
                showarrow = FALSE,
                font=list(size=11)),
           
           #Source text in the bottom left
           list(x = 1,
                y = -0.10, #position of text adjust as needed 
                text = "Source: GLA 2021-based population projections", 
                showarrow = F,
                xref='paper',
                yref='paper', 
                xanchor='right',
                yanchor='auto',
                xshift=0,
                yshift=0,
                font=list(size=11, color="grey"))))

#-------------------------------------------------------------------------------

#age structure 2036
chart_title <- "Projected age structure 2036, London"
axis_title <- "population"
f <- f+1

charts[['housing-led age structure']] <- housing_led_sya %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% c(2020,2037)) %>% 
  mutate(variant = ifelse(year == 2020, "2020 structure", variant)) %>% 
  unique() %>% 
  mutate(value = popn) %>% 
  mutate(variant = factor(variant, levels = c("2020 structure", hlm_factor_levels))) %>% 
  age_chart_plotly(chart_title, colour = "variant", figure = f,
                   percent = TRUE)

#-------------------------------------------------------------------------------

#AHS

chart_title <- "Average Household Size, London"
f <- f+1
axis_title <- "AHS"

charts[['ahs']] <- ahs %>% 
  filter(year %in% 2012:2041) %>%
  mutate(variant = factor(variant, levels = c("Past AHS", hlm_factor_levels))) %>% 
  line_chart_plotly(chart_title,
                    "year", "ahs", "variant",
                    axis_title, figure = f, zero_y = FALSE, round_y = 2,
                    source = "Source: London development database, GLA 2021-based population projections")

#-------------------------------------------------------------------------------

#Maps

boroughs <- readOGR(dsn = "W:/GISDataMapInfo/BaseMapping/Boundaries/AdminBoundaries/2019/ESRI/London",
                    layer = "London_Borough_Excluding_MHW",
                    verbose = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))

map_function <- function(data, polygons, bins, palette){
  
  polygons@data <- left_join(polygons@data, data, by="GSS_CODE")
  pal <- colorBin(palette, domain = polygons$change, bins = bins)
  labels <- sprintf(
    "<strong>%s</strong><br/>%g persons",
    polygons$NAME, polygons$change
  ) %>% lapply(htmltools::HTML)
  
  map_title <- "Population growth 2021-2041"
  
  map_output <- leaflet() %>%
    addPolygons(data = polygons,
                fillColor = ~pal(change),
                weight = 1,
                opacity = 1,
                color = "grey",
                fillOpacity = 0.8,
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>% 
    addLegend(pal = pal, values = polygons$change, opacity = 0.8,
              title = map_title,
              position = "bottomleft") 
  
  return(map_output)
  
}

bins <- c(-10000, 0, 10000, 20000, 30000, 50000, 200000)
#map_palette <- gla_colour_palette()[c(1,3:7,2)]

library(gglaplot)
map_palette <- c("black", gla_pal(palette_type = "quantitative", main_colours = "pink", n = 5))

charts[['shlaa map']] <- housing_led_change %>% 
  filter(variant == "Identified Capacity") %>% 
  mutate(change = round(change, 0)) %>% 
  map_function(boroughs, bins, map_palette)

charts[['ldd map']] <- housing_led_change %>% 
  filter(variant == "Past Delivery") %>% 
  mutate(change = round(change, 0)) %>% 
  map_function(boroughs, bins, map_palette)

charts[['lp map']] <- housing_led_change %>% 
  filter(variant == "Housing Targets") %>% 
  mutate(change = round(change, 0)) %>% 
  map_function(boroughs, bins, map_palette)

#-------------------------------------------------------------------------------

#Tables

charts[['table growth']] <-  data_list[['population']] %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% c(2021,2031,2041,2050)) %>% 
  mutate(popn = round(popn/1000000,3)) %>% 
  pivot_wider(names_from = year, values_from = popn) %>% 
  select(-gss_code) 

charts[['table working']] <-  data_list$sya %>% 
  filter(age %in% 16:64) %>% 
  group_by(variant, gss_code, year) %>% 
  summarise(popn = sum(popn), .groups = 'drop_last') %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% c(2021,2031,2041,2050)) %>% 
  mutate(popn = round(popn/1000000,3)) %>% 
  pivot_wider(names_from = year, values_from = popn) %>% 
  select(-gss_code)

charts[['ons table']] <- data_list[['ons_stage_2_households']] %>% 
  filter_london(data_col = "households") %>% 
  filter(year %in% c(2011, seq(2015,2050,5))) %>% 
  mutate(households = round(households/1000000,3)) %>% 
  pivot_wider(names_from = variant, values_from = households) %>% 
  select(-gss_code) 

charts[['dclg table']] <- data_list[['dclg_stage_2_households']] %>% 
  filter_london(data_col = "households") %>% 
  filter(year %in% c(2011, seq(2015,2050,5))) %>% 
  mutate(households = round(households/1000000,3)) %>% 
  pivot_wider(names_from = variant, values_from = households) %>% 
  select(-gss_code) 

charts[['ons annualised']] <-  data_list[['ons_stage_2_households']] %>% 
  filter_london(data_col = "households") %>% 
  filter(year %in% c(2018, 2043)) %>%
  pivot_wider(names_from = year, values_from = households) %>% 
  mutate(change = `2043`-`2018`,
         annualised = round(change/25,-1),
         households = round(`2043`,-3),
         change = round(change, -1)) %>%  
  mutate(annualised = format(annualised, big.mark=","),
         change = format(change, big.mark=","),
         households = format(households, big.mark=",")) %>% 
  select(variant, households, change, annualised) 

charts[['dclg annualised']] <-  data_list[['dclg_stage_2_households']] %>% 
  filter_london(data_col = "households") %>% 
  filter(year %in% c(2018, 2043)) %>%
  pivot_wider(names_from = year, values_from = households) %>% 
  mutate(change = `2043`-`2018`,
         annualised = round(change/25,-1),
         households = round(`2043`,-3),
         change = round(change, -1)) %>%  
  mutate(annualised = format(annualised, big.mark=","),
         change = format(change, big.mark=","),
         households = format(households, big.mark=",")) %>% 
  select(variant, households, change, annualised) 

charts[['housing-led table']] <- housing_led_popn  %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% c(2020,2030,2041)) %>% 
  mutate(popn = round(popn/1000000,3)) %>% 
  pivot_wider(names_from = year, values_from = popn) %>% 
  select(-gss_code) %>% 
  rename(scenario = variant)

#-------------------------------------------------------------------------------

dir.create("outputs/markdown", showWarnings = FALSE)
saveRDS(charts, "outputs/markdown/2021_results_doc_charts.rds")
saveRDS(data_list, "outputs/markdown/2021_results_doc_data.rds")
rm(list = ls())
