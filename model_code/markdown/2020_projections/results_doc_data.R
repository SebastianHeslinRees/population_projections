devtools::load_all("Q:/Teams/D&PA/Demography/demogtools")
source("model_code/markdown/2020_projections/demog_plotly.R")
library(dplyr)
library(data.table)
library(leaflet)
library(rgdal)
library(tidyr)

f <- 1
root <- "outputs/trend/2020/"
root_2019 <- "outputs/trend/2019/published_27_nov_2020/"
trend_factor_levels <- c("Central Lower", "Central Upper",
                         "Low Population", "High Population")

#
projections <- list('High Population' = "2020_HC_high_21-09-21_1259",
                    'Central Upper' = "2020_CC_central_upper_21-09-21_1259",
                    'Central Lower' = "2020_CH_central_lower_21-09-21_1259",
                    'Low Population' = "2020_LC_low_21-09-21_1259") %>% 
  lapply(function(x) paste0(root,x,"/"))

#
projections_2019 <- list('High Population' = "2019_variant_HC_20-11-27_1205",
                         'Central Upper' = "2019_variant_CC_20-11-27_1153",
                         'Central Lower' = "2019_variant_CH_20-11-27_1154",
                         'Low Population' = "2019_variant_LC_20-11-27_1208") %>% 
  lapply(function(x) paste0(root_2019,x,"/"))

#
components <- c("population","births","deaths","dom_in","dom_out","dom_net",
                "int_in","int_out","int_net","total_net","natural_change")

#
households <- c("ons_stage_2_households","dclg_stage_2_households")

#
housing_led <- list('Identified Capacity' = "Identified_Capacity_21-09-22_1121",
                    'Past Delivery' = "Past_Delivery_21-09-22_1121",
                    'Housing Targets' = "Housing_Targets_21-09-22_1121") %>%
  lapply(function(x) paste0("outputs/housing_led/2020/",x,"/"))

#-------------------------------------------------------------------------------

data_list <- list()
charts <- list()

for(i in components){
  print(i)
  data_list[[i]] <- read_multiple_projections(projections, i) %>% 
    mutate(variant = factor(variant, levels = trend_factor_levels))
}

print('age structure')
data_list[['sya']] <- read_multiple_projections(projections,
                                                "population",
                                                col_aggregation = c("year","gss_code","age")) %>% 
  mutate(variant = factor(variant, levels = trend_factor_levels))

projections <- lapply(projections, function(x) paste0(x,"households/"))

for(i in households){
  print(i)
  data_list[[i]] <- read_multiple_projections(projections,i,age_filter = NULL) %>% 
    mutate(variant = factor(variant, levels = trend_factor_levels))
  
}

data_list[['popn_2019']] <- read_multiple_projections(projections_2019, "population") %>% 
  mutate(projection = "2019-based")

#-------------------------------------------------------------------------------

#Covid Chart
chart_title <- "Short-term Projected Population, London"
axis_title <- "population"
f <- f+1

charts[['covid']] <- data_list[['population']] %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% 2020:2025) %>% 
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
                            text = "Source: GLA 2020-based population projections", 
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

#comparison chart
chart_title <- "Projected Population, London"
axis_title <- "population"
f <- f+1

charts[['comp']] <- data_list[['population']] %>% 
  mutate(projection = "2020-based") %>% 
  rbind(data_list[['popn_2019']]) %>% 
  filter_london(data_col = "popn") %>% #View()
  filter(variant %in% c("Central Upper", "Central Lower")) %>% 
  mutate(x = paste(projection, variant)) %>% 
  line_chart_plotly(chart_title,
                    "year", "popn", "x",
                    axis_title, figure = f, zero_y = FALSE) 

#Age structure
chart_title <- "Projected age structure 2036, London"
axis_title <- "population"
f <- f+1

charts[['age structure']] <- data_list[['sya']] %>% 
  filter_london(data_col = "popn") %>% 
  filter(year == 2036) %>%
  mutate(value = popn) %>% 
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
  line_chart_plotly(chart_title,
                    "year", "total_net", "variant",
                    axis_title, figure = f, zero_y = FALSE)


#International migration
re_factor_migration <- function(x){
  x %>% 
    filter(variant != "Central Lower") %>% 
    mutate(variant = case_when(variant == "Central Upper" ~ "central",
                               variant == "High Population" ~ "high migration",
                               variant == "Low Population" ~ "low migration",
                               TRUE ~ "TRUE")) %>% 
    mutate(variant = factor(variant, levels = c("central", "high migration", "low migration")))
}

chart_title <- "Net international migration, London"
f <- f+1

charts[['int net']] <- data_list[['int_net']] %>% 
  filter_london(data_col = "int_net") %>% 
  re_factor_migration() %>% 
  line_chart_plotly(chart_title,
                    "year", "int_net", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#

chart_title <- "International in migration, London"
f <- f+1

charts[['int in']] <- data_list[['int_in']] %>% 
  filter_london(data_col = "int_in") %>% 
  re_factor_migration() %>% 
  line_chart_plotly(chart_title,
                    "year", "int_in", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#

chart_title <- "International out migration, London"
f <- f+1

charts[['int out']] <- data_list[['int_out']] %>% 
  filter_london(data_col = "int_out") %>% 
  re_factor_migration() %>% 
  line_chart_plotly(chart_title,
                    "year", "int_out", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#Domestic migration
chart_title <- "Net domestic migration, London"
f <- f+1

charts[['dom net']] <- data_list[['dom_net']] %>% 
  filter_london(data_col = "dom_net") %>% 
  re_factor_migration() %>% 
  line_chart_plotly(chart_title,
                    "year", "dom_net", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#

chart_title <- "Domestic in migration, London"
f <- f+1

charts[['dom in']] <- data_list[['dom_in']] %>% 
  filter_london(data_col = "dom_in") %>% 
  re_factor_migration() %>% 
  line_chart_plotly(chart_title,
                    "year", "dom_in", "variant",
                    axis_title, figure = f, zero_y = FALSE)

#

chart_title <- "Domestic out migration, London"
f <- f+1

charts[['dom out']] <- data_list[['dom_out']] %>% 
  filter_london(data_col = "dom_out") %>% 
  re_factor_migration() %>% 
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

housing_led_sya <- read_multiple_projections(housing_led, "population",
                                             col_aggregation = c("gss_code","year","age"))

housing_led_change <- housing_led_popn %>% 
  filter(year %in% c(2020,2041)) %>% 
  mutate(popn = ifelse(year == 2020, popn*-1, popn)) %>% 
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

LP_dev <- readRDS("input_data/housing_led_model/borough_2020-based_london_plan.rds") %>% 
  mutate(variant = "Housing Targets")
shlaa_dev <- readRDS("input_data/housing_led_model/borough_2020-based_savills.rds") %>% 
  mutate(variant = "Identified Capacity")
LDD_dev <- readRDS("input_data/housing_led_model/borough_2020-based_ldd_mean.rds") %>% 
  mutate(variant = "Past Delivery")

dev_data <- rbind(LP_dev, shlaa_dev, LDD_dev) %>% 
  filter_london(data_col = "units") %>% 
  mutate(variant = factor(variant, levels = hlm_factor_levels))

ahs <- read_multiple_projections(housing_led, "summary", age = NULL,
                                 col_aggregation = c("gss_code","year","household_popn","households")) %>% 
  group_by(year, variant) %>% 
  summarise(ahs = sum(household_popn)/sum(households),
            .groups = 'drop_last') %>% 
  data.frame()

ahs <- readRDS(paste0(projections$`Central Lower`,"dclg_stage_1_households.rds")) %>%
  filter(substr(gss_code,1,3)=="E09",
         year <= 2020) %>% 
  group_by(year) %>% 
  summarise(ahs = sum(household_population)/sum(households)) %>% 
  data.frame() %>% 
  mutate(variant = "Past AHS") %>% 
  select(year, variant, ahs) %>% 
  rbind(filter(ahs, year == 2021,
               variant == "Identified Capacity") %>% 
          mutate(variant = "Past AHS"),
        ahs) 

#-------------------------------------------------------------------------------

#charts

#-------------------------------------------------------------------------------

#development

chart_title <- "Forecast dwellings, London"
f <- f+1
axis_title <- "dwellings"

charts[['dev chart']] <- dev_data %>% 
  filter(year %in% 2012:2041) %>%
  line_chart_plotly(chart_title,
                    "year", "units", "variant",
                    axis_title, figure = f, zero_y = FALSE,
                    source = "Source: London development database, GLA 2020-based population projections")


chart_title <- "Forecast housing stock, London"
f <- f+1
axis_title <- "dwellings"

charts[['stock']] <- dev_data %>% 
  filter(year %in% 2011:2041) %>%
  group_by(variant) %>% 
  mutate(stock = cumsum(units)) %>% 
  arrange(variant, year) %>% 
  data.frame() %>% 
  line_chart_plotly(chart_title,
                    "year", "stock", "variant",
                    axis_title, figure = f, zero_y = FALSE,
                    source = "Source: London development database, GLA 2020-based population projections")


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
  
  #Central Lower line
  add_trace(x = ~year, y = ~`Central Lower`, type = 'scatter', mode = 'lines',
            line = list(color = 'rgba(0,100,80,0.2)'),
            showlegend = TRUE, name = 'Trend Central Range', text = 'Central Lower') %>% 
  
  #Central Upper line and shading between the 2
  add_trace(y = ~`Central Upper`, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
            line = list(color = 'rgba(0,100,80,0.2)'),
            showlegend = FALSE, name = 'Central Upper', text = 'Central Upper') %>% 
  
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
                x0 = 2020, x1 = 2025,
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
           list(x = 2022.5,
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
                text = "Source: GLA 2020-based population projections", 
                showarrow = F,
                xref='paper',
                yref='paper', 
                xanchor='right',
                yanchor='auto',
                xshift=0,
                yshift=0,
                font=list(size=11, color="grey"))))

charts[['exec summary']] <- charts[['trend-vs-hlm']] %>% 
  
  #low popn line
  add_trace(x = ~year, y = ~`Low Population`, type = 'scatter', mode = 'lines',
            line = list(color = 'rgba(0,100,80,0.2)'),
            showlegend = TRUE, name = 'Trend Wide Range', text = 'Low Population') %>% 
  
  #high popn line and shading between the 2
  add_trace(y = ~`High Population`, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
            line = list(color = 'rgba(0,100,80,0.2)'),
            showlegend = FALSE, name = 'High Population', text = 'High Population') %>%
  
  layout(title = list(text = paste0("<b>Figure 1: ",chart_title,"</b>"),
                      xanchor = "left",
                      x = 0,
                      font = list(size = 12)))

#-------------------------------------------------------------------------------

#age structure 2036
chart_title <- "Projected age structure 2036, London"
axis_title <- "population"
f <- f+1

charts[['housing-led age structure']] <- housing_led_sya %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% c(2020,2036)) %>% 
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
                    source = "Source: London development database, GLA 2020-based population projections")

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
  
  map_title <- "Population growth 2020-2041"
  
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

charts[['table 1']] <-  data_list[['population']] %>% 
  filter_london(data_col = "popn") %>% 
  filter(year %in% c(2020,2030,2040,2050)) %>% 
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
saveRDS(charts, "outputs/markdown/results_doc_charts.rds")
#saveRDS(data_list, "outputs/markdown/results_doc_data.rds")
#rm(list = ls())
