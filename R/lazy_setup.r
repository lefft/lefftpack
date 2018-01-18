

#' load my most common set of packages and set plot defaults 
#' 
#' ('lazy' in the colloquial sense, *not* in the sense of lazy evaluation)
#'
#' @param packages vector of package names, 'defaults' to the five i use most
#' @param quiet logical, to suppress package startup messages 
#' @param show_message logical, for one-line console msg w recap of what happened
#' @param tim logical, `TRUE` (the default) if you want to attach `lefftpack::` when calling. **NOTE:** if someone besides me starts using this, i would be happy to change the default to `FALSE` or just remove the argument altogether. just drop me a line. 
#' @param set_plot_theme logical, if `TRUE` sets to the theme i like
#' @param font_size base size for plot theme
#' @param sparse_grid logical, if `TRUE` then removes minor grid lines in theme
#' @param stringsAsFactors logical, if `FALSE` (default), then session option for `stringsAsFactors` set to `FALSE`
#'
#' @return nothing, used only for side-effects of attaching packages and setting plot theme
#' @export 
#'
#' @examples lefftpack::lazy_setup()
lazy_setup <- function(packages=NULL, quiet=TRUE, show_message=TRUE, tim=TRUE, 
                       set_plot_theme=TRUE, font_size=14, sparse_grid=FALSE, 
                       stringsAsFactors=FALSE){
  if (!stringsAsFactors){
    options(stringsAsFactors=FALSE)
  }
  if (is.null(packages)){
    packages <- c("dplyr","magrittr","ggplot2","reshape2","lefftpack")
    if (!tim){
      packages <- packages[packages != "lefftpack"]
    }
  }
  
  session_packs_pre <- names(sessionInfo()[["otherPkgs"]])
  
  # packages requested but that are already attached
  dont_attach <- intersect(session_packs_pre, packages)
  
  if (length(dont_attach) > 0){
    if (show_message){
      message(paste0(">> these packages are already attached: \n     ", 
                     paste(dont_attach, collapse=" ")))
      packages <- packages[!packages %in% dont_attach]
    }
  }
  
  if (quiet){
    lefftpack::quiet_attach(packages, shhh="failonly")
  } else {
    lefftpack::quiet_attach(packages, shhh="loud")
  }
  
  session_packs_post <- names(sessionInfo()[["otherPkgs"]])
  
  # packages that were successfully attached
  packages_attached <- setdiff(session_packs_post, session_packs_pre)
  
  if (length(packages_attached) > 0){
    if (show_message){
      message(paste0(
        ">> these packages are now attached: \n     ", 
        paste(packages_attached, collapse=" ")
      ))
    }
  } else {
    if (show_message){
      message(paste0(
        ">> no new packages were attached. ", 
        paste(packages, collapse=" ")
      ))
    }
  }
  
  
  if (set_plot_theme){
    
    if ("ggplot2" %in% session_packs_post){
      prev_theme <- theme_get()
      
      tim_theme <- 
        theme_minimal(font_size) + theme(
          legend.position="top", 
          legend.title=element_blank(), 
          legend.key=element_rect(size=rel(.15)),
          legend.key.size=unit(1, "lines"), 
          axis.title=element_text(size=rel(.85)),
          axis.ticks=element_line(size=rel(.5)), 
          axis.line=element_line(size=rel(.5))
        )
      if (sparse_grid){
        tim_theme <- tim_theme + theme(panel.grid.minor=element_blank())
      }
      theme_set(tim_theme)
      if (show_message){
        # if the theme was already set, dont send a message
        if (!identical(prev_theme, tim_theme)){
          message(">> custom `ggplot2::` plot theme is now set")
        }
      }
    } else {
      if (show_message){
        # if set_plot_theme==TRUE but ggplot2 isn't loaded:
        message(">> must attach `ggplot2` to set plot theme")
      }
    }
  }
  # no return value -- this fonc is purely for its side-effects
}

