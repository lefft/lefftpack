

#' get inventory of attached, installed, loaded, and base packages 
#'
#' @param package_type can be 'attached', 'installed', 'base', 'loaded', 'all'
#'
#' @return a vector of package names (or a sparse df with a column listing package names for each package type, if `package_type='all'`)
#' @export
#'
#' @examples check_packs("attached")   # to see what's directly available
#' check_packs("installed")   # to see what's in your library 
check_packs <- function(package_type="attached"){
  # TODO: 
  #   - add ability to search one of the vectors for a particular package
  #     (e.g. just ask if dplyr:: is attached/installed/etc.)
  
  if (!package_type %in% c("attached","installed","base","loaded","all")){
    message(paste0(">> argument `package_type` must be one of: \n  ", 
                   '"attached","installed","base","loaded","all"'))
    stop()
  }
  # NOTE: for speed, only compute all four up front if package_type="all"
  if (package_type == "all"){
    packs_attached  <- names(sessionInfo()[["otherPkgs"]])
    packs_base      <- sessionInfo()[["basePkgs"]]
    packs_loaded    <- names(sessionInfo()[["loadedOnly"]])
    packs_installed <- library()$results[, 1]
    
    # get max length to add trailing na's so we can return result as a df
    pack_list <- list(packs_attached=packs_attached, 
                      packs_base=packs_base, 
                      packs_loaded=packs_loaded,
                      packs_installed=packs_installed)
    max_len <- max(sapply(pack_list, length))
    
    return(as.data.frame(lapply(pack_list, function(x){
      c(x, rep(NA, times=(max_len - length(x))))
    })))
  }
  
  if (package_type == "attached"){
    packs_attached  <- names(sessionInfo()[["otherPkgs"]])
    return(packs_attached)
  }
  
  if (package_type == "installed"){
    packs_installed <- library()$results[, 1]
    return(packs_installed)
  }
  
  if (package_type == "base"){
    packs_base      <- sessionInfo()[["basePkgs"]]
    return(packs_base)
  }
  
  if (package_type == "loaded"){
    packs_loaded <- names(sessionInfo()[["loadedOnly"]])
    return(packs_loaded)
  }
}

