# ===========================================
#
# A place for reusuable code internal to the
# popmodules package. For any short functions
# that are reused across the package but which
# aren't exported.
#
# ===========================================

# A useful package for visualising function dependencies
# is `pkgnet` https://cran.r-project.org/web/packages/pkgnet/vignettes/pkgnet-intro.html








# Function: convert character vector (unnamed or partially named) to one where
# every element is named
#
# e.g. c("a", two=b, "c") will become c(a="a", two=b, c="c")
#
# Used repeatedly in the package, usually to standardise mappings between data
# frame columns to allow for reliable joins
convert_to_named_vector <- function(vec) {
  assert_that(is.vector(vec) | is.factor(vec),
              msg = "convert_to_named_vector needs a vector or a factor as input")

  if(is.null(names(vec))) {
    names(vec) <- vec
  } else {
    ix <- names(vec) == ""
    names(vec)[ix] <- vec[ix]
  }

  return(vec)
}





# ------------------------------------------------------------------------------------

# Function:  copy the factor structure of one data frame to another

# Given source and target data frames and a mapping between common columns, find
# which columns in the first data frame are(n't) factors and convert columns in
# the second data frame to match (preserving factor ordering)

# Returns the *target* data frame, i.e. the second input parameter. Watch out
# (sorry).

# The function curently ignores factor levels (i.e. doesn't try to preserve them
# if they've changed), and doesn't check whether the input factor was ordered or not

# Used in the package to make sure that the output of various functions
# preserves the factoring of the input

match_factors <- function(dfsource, dftarget, col_mapping) {
  col_mapping <- convert_to_named_vector(col_mapping)
  for(i in  seq_along(col_mapping)) {
    icol <- col_mapping[i]
    source_col <- dfsource[[names(icol)]]
    target_col <- dftarget[[icol]]

    if(is.factor(source_col) & !is.factor(target_col)) {
      dftarget[[icol]] <- factor(dftarget[[icol]])
    }

    if(!is.factor(source_col) & is.factor(target_col)) {
      col_class <- class(source_col)
      if(col_class == "numeric") {
        dftarget[[names(icol)]] <- as.numeric(levels(target_col)[target_col])
      } else {
        dftarget[[names(icol)]] <- as.character(target_col)
      }
    }

    if(is.factor(source_col)) {
      if(!identical(levels(source_col), levels(dftarget[[icol]]))) {
        warning(paste("match_factors was given a source and target with different levels in",
                      col_mapping[i],"- watch out: any subsequent joins between the two will be coerced to character vectors"))
      }
    }
  }
  return(dftarget)
}
