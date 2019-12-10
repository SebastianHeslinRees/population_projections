#' Test a column in a dataframe for negatives, optionally setting to zero
#'
#' If negatives exist print a warning. If the \code{set_to_zero} parameter is
#' TRUE then change the negatives to 0.
#'
#' @param x A data frame.
#' @param data_col String. The name of a column in the data frame containing
#'   numeric data.
#' @param set_to_zero Logical. When TRUE, set negative values to zero. When
#'   FALSE return the data frame as is.
#'
#' @return The input data frame. If \code{set_to_zero} is TRUE, negative values
#'   of \code{data_col} are set to zero.
#'
#' @export

check_negative_values <- function(x, data_col, set_to_zero = TRUE){

  assertthat::assert_that(data_col %in% names(x))
  assertthat::assert_that(any(c("numeric", "integer") %in% class(x[[data_col]])),
                          msg = paste("check_negative_values was passed a non-numeric column:", data_col))


  ix <- x[[data_col]] < 0

  if(any(ix)) {

    zero_msg <- ifelse(set_to_zero == TRUE,
                       "Setting negative values to zero.",
                       "Warning only, returning dataframe unchanged.")

    n <- sum(ix)
    total_negative <- sum(x[[data_col]][ix])

    warning(paste0(capture.output({
      print(paste0("Negative values of ", data_col, " found at ",
                   n, " aggregation levels summing to ",
                   total_negative, ". ", zero_msg))
      if(sum(ix) < 30) {
        print("Values:")
        print(x[ix,])
      } else {
        print("First 30 values:")
        print(x[ix,][1:30,])
      }
    }), collapse = "\n"))

    if(set_to_zero){
      x[[data_col]][ix] <- 0
    }
  }

  return(x)
}
