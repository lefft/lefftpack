

#' load data associated with this package
#'
#' @return a toy data frame loaded from a csv in the package
#' @export
#'
#' @examples boosh <- load_lefftpack_data()
load_lefftpack_data <- function(){
  read.csv("inst/extdata/boosh.csv", stringsAsFactors=FALSE)
}

