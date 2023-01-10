library(plotly)

line_chart_plotly <- function(data, title, x, y, colour,
                              y_axis_title, zero_y=FALSE,
                              figure = NULL, round_y = 0,
                              source = NULL){
  
  data <- data %>% rename(x_axis_col = all_of(x))
  data <- data %>% rename(y_axis_col = all_of(y))
  data <- data %>% rename(colour_col = all_of(colour))
  
  hvr_tmp <- paste0("<b>%{x}</b><br>","%{text}: %{y:,.",round_y,"f}","<extra></extra>")
  
  if(is.null(source)){
    source_text <- "Source: GLA 2020-based population projections"
  } else {
    source_text <- source
  }
  
  #Basic Plot
  p <- plot_ly(data, x=~x_axis_col, y=~y_axis_col, color=~colour_col,
               colors = c("#6da7de",
                          "#9e0059",
                          "#dee000"),
               type = "scatter",
               mode = "lines+markers", 
               line = list(shape = "spline"),
               text = ~colour_col,
               hovertemplate = hvr_tmp)#,
               # width = 900,
               # height = 600) 
  
  #x-axis to zero
  if(zero_y){
    y_options <- list(title = y_axis_title,
                      showgrid = TRUE,
                      rangemode = "tozero")
    y_min_y_axis_col <- 0
  } else {
    y_options <- list(title = y_axis_title,
                      showgrid = TRUE)
    y_min_y_axis_col <- min(data$y_axis_col)
  }
  
  #Global layout and title details
  title <- ifelse(is.null(figure),
                  paste0("<b>",title,"</b>"),
                  paste0("<b>Figure ",figure,": ",title,"</b>"))
  p %>% 
    layout(title = list(text = title,
                        xanchor = "left",
                        x = 0,
                        font = list(size = 12)),
           xaxis = list(title = "",
                        showgrid = FALSE),
           yaxis = y_options,
           legend = list(orientation = "h"),
           margin = list(b=50),
           autosize = TRUE,
           
           #3 rectangles for different periods
           shapes = list(
             list(type = "rect",
                  fillcolor = "#44aad5",
                  line = list(width = 0),
                  opacity = 0.15,
                  x0 = 2020, x1 = 2025,
                  y0 = y_min_y_axis_col, y1 = max(data$y_axis_col),
                  layer = "below"),
             # list(type = "rect",
             #      fillcolor = "#44aad5",
             #      line = list(width = 0),
             #      opacity = 0.1,
             #      x0 = 2022, x1 = 2025,
             #      y0 = y_min_y_axis_col, y1 = max(data$y_axis_col),
             #      layer = "below"),
             list(type = "rect",
                  fillcolor = "#44aad5",
                  line = list(width = 0),
                  opacity = 0.05,
                  x0 = 2025, x1 = max(data$x_axis_col+1),
                  y0 = y_min_y_axis_col, y1 = max(data$y_axis_col),
                  layer = "below")),
           
           #Annotoation at the top of the rectangles
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
             
             # list(x = 2024.5,
             #      y = 0.93,
             #      text = "Transition\nperiod",
             #      xref = "x",
             #      yref = "paper",
             #      xanchor = 'center',
             #      yanchor = 'top',
             #      showarrow = FALSE,
             #      font=list(size=11)),
             
             list(x = mean(c(2025,max(data$x_axis_col))),
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
                  text = source_text, 
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

age_chart_plotly <- function(data, title, colour, figure=NULL, percent=FALSE){
  
  title <- ifelse(is.null(figure),
                  paste0("<b>",title,"</b>"),
                  paste0("<b>Figure ",figure,": ",title,"</b>"))
  
  data <- data %>% rename(colour_col = all_of(colour))
  
  if(percent){
    hvr <- paste("<b>Age %{x}</b><br>",
          "%{text}: %{y:,.2f}",
          "<extra></extra>")
  } else {
    hvr <- paste("<b>Age %{x}</b><br>",
                 "%{text}: %{y:,.0f}",
                 "<extra></extra>")
  }

  #Basic plot
  plot_ly(data, x=~age, y=~value, color=~colour_col,
          colors = c("#6da7de",
                     "#9e0059",
                     "#dee000"),
          type = "scatter",
          mode = "lines+markers", 
          line = list(shape = "spline"),
          text = ~colour_col,
          hovertemplate = hvr) %>% 
    
    #Global layout and title details
    layout(title = list(text = title,
                        xanchor = "left",
                        x = 0,
                        font = list(size = 12)),
           xaxis = list(title = "",
                        showgrid = FALSE,
                        zeroline = FALSE,
                        showticklabels = TRUE,
                        dtick = 10),
           yaxis = list(title = "population",
                        showgrid = TRUE),
           legend = list(orientation = "h"),
           margin = list(b=100),
           
           #Source text in the bottom left
           annotations = list(
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
                  font=list(size=11, color="grey"))
           )
    )
  
}
