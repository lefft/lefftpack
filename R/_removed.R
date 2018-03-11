

#' compare the speed of two expressions  [DEFUNCT -- JUST USE MICROBENCHMARK]
#' 
#' NOTE: args must be passed as strings! (for now...)
#'
#' @param expr1 a quoted piece of code 
#' @param expr2 another quoted piece of code 
#'
#' @return length-two vector with names equal to `expr1` and `expr2`, and values equal to the elapsed time after execution of each 
#'
#' @examples 
#' compare_speed(expr1="length(1:(10^8))", expr2="length(1:(10^7))")
#' boosh <- rep(c("bah", "bahbah", "boosh!"), times=1e5)
#' compare_speed(expr1="sum(grepl('ba', boosh))", 
#'               expr2="sum(grepl('oo', boosh))") 
compare_speed <- function(expr1, expr2){ 
  if (!all(is.character(expr1), is.character(expr2))){
    message("you gotta pass args as character")
    return(NULL)
  } 
  # message("assuming inputs are quoted code\n   ~~> will improve later")
  out_names <- c(as.character(expr1), as.character(expr2))
  out <- c(system.time(eval(parse(text=expr1)))["elapsed"], 
           system.time(eval(parse(text=expr2)))["elapsed"])
  return(setNames(object=out, nm=out_names))
} 
