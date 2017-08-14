
#' attach packages w/o annoying messages (legacy version)
#'
#'
#' @param packages 
#' @param shhh 
#'
#' @return val 
#' @export
#'
#' @examples boosh 
quiet_attach_old <- function(packages, shhh=TRUE){
  # TODO: get lefftpack to take a list of inputs
  for (x in seq_along(packages)){
    if (shhh==TRUE){
      suppressPackageStartupMessages(
        do.call(library, list(packages[x]))
      )
    } else {
      do.call(library, list(packages[x]))
    }
  }
}
