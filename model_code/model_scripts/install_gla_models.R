#' Install the GLA model packages
#' 
#' Run devtools::document() and devtools::intall() on the GLA model packages for
#' first time installation or package updates. Specifiy which packages should be
#' updated using function parameters.
#' 
#' @param popmodules Logical Document and install the popmodules package Default \code{TRUE}
#' @param trend Logical Document and install the trendmodel package Default \code{TRUE}
#' @param housingled Logical Document and install the housingledmodel package Default \code{TRUE}
#' @param small_area Logical Document and install the smallareamodel package Default \code{TRUE}

install_gla_models <- function(popmodules = TRUE, trend = TRUE, housing_led = TRUE, small_area = TRUE){
  
  if(popmodules){
    devtools::document("model_code/popmodules")
    devtools::install("model_code/popmodules", upgrade = FALSE)
  }
  
  if(trend){
    devtools::document("model_code/trendmodel")
    devtools::install("model_code/trendmodel", upgrade = FALSE)
  }
  
  if(small_area){
    devtools::document("model_code/smallareamodel")
    devtools::install("model_code/smallareamodel", upgrade = FALSE)
  }
  
  if(housing_led){
    devtools::document("model_code/housingledmodel")
    devtools::install("model_code/housingledmodel", upgrade = FALSE)
  }
  
  p <- ifelse(popmodules, "popmodules, ", "")
  t <- ifelse(trend, "trendmodel, ", "")
  h <- ifelse(housing_led, "housingledmodel, ", "")
  s <- ifelse(small_area, "smallareamodels, ", "")
  
  message(paste0(p, t, h, s, "documented and installed"))
  
}
