#library(highcharter)
library(plotly)
library(ggplot2)
#source('Q:/Teams/D&PA/Demography/demogtools/R/demogtools.R')

# basic_chart_hc <- function(data, title, y_axis, area_name){
#   
#   data %>% 
#     mutate(label = format(round(value,0),big.mark=",")) %>% 
#     
#     hchart("spline",
#            hcaes(x = year, y = value, group = variant),
#            showInLegend = TRUE,
#            tooltip = list(pointFormat = "<b>{point.variant}:</b> {point.label}")) %>% 
#     hc_legend(align = "top", verticalAlign = "top", layout = "horizontal") %>% 
#     hc_yAxis(title = list(text = y_axis)) %>% 
#     hc_xAxis(title = list(text = "year"), min = 2010,
#              plotBands = list(
#                list(
#                  label = list(text = "Covid </br>Period"),
#                  color = "rgba(68, 170, 213, 0.15)",
#                  from = 2019,
#                  to = 2022
#                ),
#                list(
#                  label = list(text = "Transition </br>period"),
#                  color = "rgba(68, 170, 213, 0.1)",
#                  from = 2022,
#                  to = 2028
#                ),
#                list(
#                  label = list(text = "Long-term </br>period"),
#                  color = "rgba(68, 170, 213, 0.05)",
#                  from = 2028,
#                  to = 2050))) %>%
#     hc_plotOptions(
#       series = list(
#         boderWidth = 0,
#         dataLabels = list(enabled = FALSE)),
#       column = list(stacking = "normal")) %>% 
#     hc_title(text = paste0(title, ", ",area_name), align = "center") %>% 
#     hc_subtitle(
#       text = "2019-based BPO projections", align = "center") %>%
#     hc_caption(
#       text = "Chart: GLA demography") %>% 
#     hc_credits(
#       enabled = TRUE, text = "Source: GLA 2019-based BPO projections",
#       href = "https://data.london.gov.uk/dataset/trend-based-population-projections",
#       style = list(fontSize = "12px")) %>% 
#     hc_size(
#       height = 600,
#       width = 1000) %>% 
#     hc_exporting(
#       enabled = TRUE, # always enabled
#       filename = paste0("2019-bpo-data_",area_name),
#       showTable = FALSE) %>% 
#     hc_colors(c("#6da7de",
#                 "#9e0059",
#                 "#dee000"))
#   
# }
# 
# age_chart_hc <- function(data, area_name, var){
#   
#   title <- "Age structure"
#   y_axis <- "persons"
#   
#   data %>% 
#     mutate(label = format(round(value,0),big.mark=",")) %>% 
#     filter(variant == var) %>% 
#     
#     hchart("spline",
#            hcaes(x = age, y = value, group = year),
#            showInLegend = TRUE,
#            tooltip = list(pointFormat = "<b>{point.year}:</b> {point.label}")) %>% 
#     hc_legend(align = "top", verticalAlign = "top", layout = "horizontal") %>% 
#     hc_yAxis(title = list(text = y_axis)) %>% 
#     hc_xAxis(title = list(text = "age"), min = 0) %>%
#     hc_plotOptions(
#       series = list(
#         boderWidth = 0,
#         dataLabels = list(enabled = FALSE)),
#       column = list(stacking = "normal")) %>% 
#     hc_title(text = paste0(title, ", ",area_name), align = "center") %>% 
#     hc_subtitle(
#       text = "2019-based BPO projections", align = "center") %>%
#     hc_caption(
#       text = "Chart: GLA demography") %>% 
#     hc_credits(
#       enabled = TRUE, text = "Source: GLA 2019-based BPO projections",
#       href = "https://data.london.gov.uk/dataset/trend-based-population-projections",
#       style = list(fontSize = "12px")) %>% 
#     hc_size(
#       height = 600,
#       width = 1000) %>% 
#     hc_exporting(
#       enabled = TRUE, # always enabled
#       filename = paste0("2019-bpo-data_",area_name),
#       showTable = FALSE) %>% 
#     hc_colors(c("#6da7de",
#                 "#9e0059",
#                 "#dee000"))
#   
# }

line_chart_plotly <- function(data, title, y_axis, area_name, zero_y=FALSE){
  
  #Basic Plot
  p <- plot_ly(data, x=~year, y=~value, color=~variant,
               colors = c("#6da7de",
                          "#9e0059",
                          "#dee000"),
               type = "scatter",
               mode = "lines+markers", 
               line = list(shape = "spline"),
               text = ~variant,
               hovertemplate = paste("<b>%{x}</b><br>",
                                     "%{text}: %{y:,.0f}",
                                     "<extra></extra>"),
               width = 1000,
               height = 600) 
  
  #x-axis to zero
  if(zero_y){
    y_options <- list(title = y_axis,
                      showgrid = TRUE,
                      rangemode = "tozero")
    y_min_value <- 0
  } else {
    y_options <- list(title = y_axis,
                      showgrid = TRUE)
    y_min_value <- min(data$value)
  }
  
  #Global layout and title details
  p %>% 
    layout(title = paste0(title, ", ",area_name),
           xaxis = list(title = "",
                        showgrid = FALSE),
           yaxis = y_options,
           legend = list(orientation = "h"),
           margin = list(b=50),
           
           #3 rectangles for different periods
           shapes = list(
             list(type = "rect",
                  fillcolor = "#44aad5",
                  line = list(width = 0),
                  opacity = 0.15,
                  x0 = 2019, x1 = 2022,
                  y0 = y_min_value, y1 = max(data$value),
                  layer = "below"),
             list(type = "rect",
                  fillcolor = "#44aad5",
                  line = list(width = 0),
                  opacity = 0.1,
                  x0 = 2022, x1 = 2027,
                  y0 = y_min_value, y1 = max(data$value),
                  layer = "below"),
             list(type = "rect",
                  fillcolor = "#44aad5",
                  line = list(width = 0),
                  opacity = 0.05,
                  x0 = 2027, x1 = max(data$year+1),
                  y0 = y_min_value, y1 = max(data$value),
                  layer = "below")),
           
           #Annotoation at the top of the rectangles
           annotations = list(
             list(x = 2020.5,
                  y = 0.93,
                  text = "Covid\nperiod",
                  xref = "x",
                  yref = "paper",
                  xanchor = 'center',
                  yanchor = 'top',
                  showarrow = FALSE,
                  font=list(size=11)),
             
             list(x = 2024.5,
                  y = 0.93,
                  text = "Transition\nperiod",
                  xref = "x",
                  yref = "paper",
                  xanchor = 'center',
                  yanchor = 'top',
                  showarrow = FALSE,
                  font=list(size=11)),
             
             list(x = mean(c(2028,max(data$year))),
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
                  text = "Source: GLA 2019-based BPO projections", 
                  showarrow = F,
                  xref='paper',
                  yref='paper', 
                  xanchor='right',
                  yanchor='auto',
                  xshift=0,
                  yshift=0,
                  font=list(size=11, color="grey"))
           )
           
           
    )
}

age_chart_plotly <- function(data, area_name, var){
  
  title <- "Age structure"
  y_axis <- "persons"
  
  data <- data %>%
    filter(variant == var) %>% 
    arrange(age) %>% 
    mutate(year = as.character(year))
  
  #Basic plot
  plot_ly(data, x=~age, y=~value, color=~year,
          colors = c("#6da7de",
                     "#9e0059",
                     "#dee000"),
          type = "scatter",
          mode = "lines+markers", 
          line = list(shape = "spline"),
          text = ~year,
          hovertemplate = paste("<b>Age %{x}</b><br>",
                                "%{text}: %{y:,.0f}",
                                "<extra></extra>"),
          width = 1000,
          height = 600) %>% 
    
    #Global layout and title details
    layout(title = paste0(title, ", ",area_name,"\n",var),
           xaxis = list(title = "",
                        showgrid = FALSE,
                        zeroline = FALSE,
                        showticklabels = TRUE,
                        dtick = 10),
           yaxis = list(title = y_axis,
                        showgrid = TRUE),
           legend = list(orientation = "h"),
           margin = list(b=100),
           
           #Source text in the bottom left
           annotations = list(
             list(x = 1,
                  y = -0.10, #position of text adjust as needed 
                  text = "Source: GLA 2019-based BPO projections", 
                  showarrow = F,
                  xref='paper',
                  yref='paper', 
                  xanchor='right',
                  yanchor='auto',
                  xshift=0,
                  yshift=0,
                  font=list(size=11, color="grey"))
           )
    )
  
}

# age_chart_plotly(x, "cheeseballs", "scenario 1")
# 
# age_chart_hc(x, "cheeseballs", "scenario 1")
# 
# x <- expand.grid(age = 0:90, year = 2011:2013, variant = c("scenario 1","scenario 2")) %>% 
#   mutate(value = runif(546,0,100)) %>% 
#   data.frame()


# basic_chart_plotly(data, title, y_axis, area_name)
# 
# data <- readRDS("C:/Projects_c/population_projections_c/notebooks_and_analysis/dummy_data.rds") %>%
#   filter(substr(variant,1,1)=="h") %>%
#   mutate(variant = substr(variant,6,6),
#          variant = paste("scenario",variant)) %>%
#   rename(value = popn)
# 
# area_name <- "London"
# title <- "BPO Variants"
# y_axis <- "population"
# 
# basic_chart_hc(data, title, y_axis, area_name)
# #basic_chart_ggplotly(data, title, y_axis, area_name)
# basic_chart_plotly(data, title, y_axis, area_name)

bar_chart_plotly <- function(data, title, y_axis, area_name){
  
  #Basic Plot
  plot_ly(data, x=~year, y=~value, color=~variant,
          colors = c("#6da7de",
                     "#9e0059",
                     "#dee000"),
          type = "bar",
          text = ~variant,
          hovertemplate = paste("<b>%{x}</b><br>",
                                "%{text}: %{y:,.0f}",
                                "<extra></extra>"),
          width = 1000,
          height = 600) %>%  
    
    #Global layout and title details
    layout(title = paste0(title, ", ",area_name),
           xaxis = list(title = "",
                        showgrid = FALSE),
           yaxis = list(title = y_axis,
                        showgrid = TRUE),
           legend = list(orientation = "h"),
           margin = list(b=50),
           
           #3 rectangles for different periods
           shapes = list(
             list(type = "rect",
                  fillcolor = "#44aad5",
                  line = list(width = 0),
                  opacity = 0.15,
                  x0 = 2019.5, x1 = 2022.5,
                  y0 = min(data$value), y1 = max(data$value),
                  layer = "below"),
             list(type = "rect",
                  fillcolor = "#44aad5",
                  line = list(width = 0),
                  opacity = 0.1,
                  x0 = 2022.5, x1 = 2027.5,
                  y0 = min(data$value), y1 = max(data$value),
                  layer = "below"),
             list(type = "rect",
                  fillcolor = "#44aad5",
                  line = list(width = 0),
                  opacity = 0.05,
                  x0 = 2027.5, x1 = 2051,
                  y0 = min(data$value), y1 = max(data$value),
                  layer = "below")),
           
           #Annotoation at the top of the rectangles
           annotations = list(
             list(x = 2021,
                  y = 0.93,
                  text = "Covid\nperiod",
                  xref = "x",
                  yref = "paper",
                  xanchor = 'center',
                  yanchor = 'top',
                  showarrow = FALSE,
                  font=list(size=11)),
             
             list(x = 2025,
                  y = 0.93,
                  text = "Transition\nperiod",
                  xref = "x",
                  yref = "paper",
                  xanchor = 'center',
                  yanchor = 'top',
                  showarrow = FALSE,
                  font=list(size=11)),
             
             list(x = 2039,
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
                  text = "Source: GLA 2019-based BPO projections", 
                  showarrow = F,
                  xref='paper',
                  yref='paper', 
                  xanchor='right',
                  yanchor='auto',
                  xshift=0,
                  yshift=0,
                  font=list(size=11, color="grey"))
           )
           
           
    )
}