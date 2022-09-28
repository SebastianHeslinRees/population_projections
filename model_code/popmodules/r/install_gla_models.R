#' Install the GLA model packages
#' 
#' Run devtools::document() and devtools::install() on the GLA model packages for
#' first time installation or package updates. Specify which packages should be
#' updated using function parameters.
#' 
#' @param trend Logical Document and install the trendmodel package Default \code{TRUE}
#' @param housing_led Logical Document and install the housingledmodel package Default \code{TRUE}
#' @param flex_area Logical Document and install the flexibleareamodel package Default \code{TRUE}
#' 
#' @importFrom devtools document install
#' 
#' @export

install_gla_models <- function(trend = TRUE, housing_led = TRUE, flex_area = TRUE){
  
  if(trend){
    devtools::document("model_code/trendmodel")
    devtools::install("model_code/trendmodel", upgrade = FALSE)
  }
  
  if(flex_area){
    devtools::document("model_code/flexibleareamodel")
    devtools::install("model_code/flexibleareamodel", upgrade = FALSE)
  }
  
  if(housing_led){
    devtools::document("model_code/housingledmodel")
    devtools::install("model_code/housingledmodel", upgrade = FALSE)
  }
  
  t <- ifelse(trend, "trendmodel, ", "")
  h <- ifelse(housing_led, "housingledmodel, ", "")
  s <- ifelse(flex_area, "flexibleareamodel, ", "")
  
  message(paste0(t, h, s, "documented and installed"))
  
}
