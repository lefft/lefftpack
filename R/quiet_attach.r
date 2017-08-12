

#' attach packages w/o annoying messages
#'
#' @param ... package names, as characters. can be `...` or a vector
#' @param shhh 'failonly' (the default) or 'silent' or 'quiet' or 'loud'. throws exception if another value is supplied.
#'
#' @return nothing, used only for side-effects of attaching packages and producing console messages about (un)available packages you tried to attach (if shhh!='silent')
#' @export 
#'
#' @examples quiet_attach("dplyr", "lefftpack", "ballr", shhh="failonly")
quiet_attach <- function(..., shhh="failonly"){
  stopifnot(shhh %in% c("silent","quiet","failonly","loud"))
  # the things we want to attach
  packages  <- unlist(list(...))
  # all the things we *could* attach
  available <- unname(installed.packages()[,1])
  # the ones we want to attach but cant
  dont_gots <- packages[!packages %in% available]
  # aaaand just the ones we can attach
  packages <- packages[!packages %in% dont_gots]
  # to disable the quietness, just attach them all
  if (shhh=="loud"){
    for (x in seq_along(packages)){
      do.call(library, list(packages[x]))
    }
    if (length(dont_gots) > 1){
      message(paste0(
        "these must be installed before you can attach them:\n  ", 
        list(dont_gots)
      ))
    }
    if (length(packages) > 1){
      message(paste0(
        "packages now available for direct use:\n  ", list(packages)
      ))
    }
    # aaaand if shhh=="loud", then we're donezo
  } else {
    # otherwise (if shhh=="quiet"), break the news...
    if (shhh!="silent"){
      if (length(dont_gots) > 0){
        message(paste0(
          "these must be installed before you can attach them:\n  ", 
          list(dont_gots)
        ))
      }
    }
    # and then loop over the paqqs and queitly attach each one 
    for (x in seq_along(packages)){
      suppressPackageStartupMessages(
        do.call(library, list(packages[x]))
      )
    }
    # unless shhh=="failonly", then say which the ones we attached
    if (shhh=="quiet"){
      if (length(packages) > 0){
        message(paste0(
          "packages now available for direct use:\n  ", list(packages)
        ))
      }
    }
  }
  # no return value -- this fonc is purely for its side-effects
}

### TESTING AREA FOR `quiet_attach()`
# possible properties of param `...` values: 
#   N: none of `...` are installed
#   A: all of `...` are installed
#   S: some of `...` are installed
# 
# possible vals for param `shhh`, and desired behavior: 
#   on `shhh=="silent"`, say smthg if:     --
#   on `shhh=="failonly"`, say smthg if:   N, S
#   on `shhh=="quiet"`, say smthg if:      N, A, S
#   on `shhh=="loud"`, say smthg if:       N, A, S
# 
# 
# QUESTION: do we care abt cases where smthg is already attached?!?! dk...
# 
# 
### TESTS FOR `quiet_attach()` 
# 
# ## testing case N
# qa("blah","boosh","blaowwie", shhh="loud")
# qa("blah","boosh","blaowwie", shhh="quiet")
# qa("blah","boosh","blaowwie", shhh="failonly")
# qa("blah","boosh","blaowwie", shhh="silent")
# qa("blah","boosh","blaowwie", shhh="AN UNINTENDED VALUE")
# 
# ## testing case S
# qa("dplyr","boob","blaowwie", shhh="loud")
# qa("dplyr","boob","blaowwie", shhh="quiet")
# qa("dplyr","boob","blaowwie", shhh="failonly")
# qa("dplyr","boob","blaowwie", shhh="silent")
# qa("dplyr","boob","blaowwie", shhh="AN UNINTENDED VALUE")
# 
# ## testing case A
# qa("dplyr","ballr","foreign", shhh="loud")
# qa("dplyr","ballr","foreign", shhh="quiet")
# qa("dplyr","ballr","foreign", shhh="failonly")
# qa("dplyr","ballr","foreign", shhh="silent")
# qa("dplyr","ballr","foreign", shhh="AN UNINTENDED VALUE")



