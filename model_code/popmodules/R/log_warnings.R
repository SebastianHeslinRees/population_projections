#' Write warnings to a text file
#' 
#' @param file_path A string giving the directory path and the name
#' of the file to save..
#'
#' @export
#' 
log_warnings <- function(file_path){
  
  z <- attr(warnings(), "names")
  n <- data.frame()
  
  for(m in seq(z)){
    n[m,1] <- z[[m]]
  }
  
  if(nrow(n) >= 50){
    n <- data.frame(V1 = "More than 50 warnings were recorded. Showing the first 50 only.") %>%
      rbind(n)
  }
  
  data.table::fwrite(n, file_path, col.names=FALSE)

}

