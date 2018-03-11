### functions for data analysis and other misc stuff 
### (updated jun21/2017)
# 
## wishlist: 
# 
#   -x- - center a variable around zero
#   - bootstrap regression coefficients
#   - simple permutation test func
#   - more general perm func
#   - need to represent `colz()` as data not fonc
#   - import a single func/obj from a *package* (import does for files)
#   - TODO -- fix @example tags, when wont run w/o specific file available


#' python-style import
#' 
#' import a single function from a file (instead of getting them all with `source()`)
#'
#' @param what function to be imported (*not quoted*)
#' @param from path to file that defines `what` (character)
#' @param msg whether to print a message on success ("func X was imported"); defaults to `TRUE`
#'
#' @return no return value 
#' @export
#'
#' @examples
#' # usage example (assumes `boosh()` is defined in "util.r")
#' import(what=boosh, from="util.r") # `boosh()` gets created in calling env
#' rm(boosh) # clear `boosh()` from ws
#' blaowwie <- function(){
#'   import(boosh, from="util.r"); boosh("blah1","blah2")}
#' blaowwie() # `boosh()` will *not* be created in calling env of `blaowwie()` 
import <- function(what, from, msg=TRUE){
  # TODO: 
  #   - generalize for importing multiple objs
  #   - enable `from X import *` (basically just `source()`)
  #   - allow for `import X as <whatev>` 
  #   - allow for collect everything into a list + assign in parent env
  #   - better filename handling?
  #   - allow passing func names as character in addition to raw
  #     (the strat is: gsub("^\"|\"$", "", deparse(substitute("<WHAT-HERE>"))))
  #   - allow specifying environment to import to (currently just parent)
  #   - allow for importing non-funcs 
  #   - be smarter -- first look for the func, then use regex to grab it
  #   - in smarter approach, need to worry about deps/other funcs called
  
  what <- deparse(substitute(expr=what))
  env <- new.env(parent=parent.frame())
  
  source(file=from, local=env)
  
  if (what %in% names(env)){
    func <- eval(parse(text=what), envir=env)
    assign(what, value=func, envir=parent.env(env))
    if (msg){
      param_names <- names(formals(func))
      func_sig <- paste0(what, "(", paste(param_names, collapse=", "), ")")
      message(">> imported `", func_sig, "` from `", from, "`")
    }
    
  }
  else message(">> no func `", what, "` to import from `", from, "`")
}






# standard error of a sample proportion
#' se of a proportion
#'
#' @param p 
#' @param n 
#'
#' @return val 
#' @export
#'
#' @examples boosh 
se_prop <- function(p, n){
  round(sqrt((p*(1-p))/n))
}

# reasonable color palette for plotting
#' colorz
#'
#' @return val 
#' @export
#'
#' @examples colorz()['lite_tan'] 
colorz <- function(){
  c(lite_tan="#dbd4d0", claudy_braun="#afa69e", afghan_tan="#c8ab94",
    baygish="#68442a", bilbao="#248100", japlore="#2F742C")
}


#' quick zscore func 
#'
#' equivalent to a wrapper around `scale()` w defaults, but returns atomic vec
#'
#' @param x 
#'
#' @return val 
#' @export
#'
#' @examples boosh 
zscore <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}




#' pval_classify 
#' 
#' annotate vector of p-values w stars, indicating common signif levels
#' 
#' @param x 
#' @param alpha 
#'
#' @return val 
#' @export
#'
#' @examples boosh 
pval_classify <- function(x, alpha=.05){
  ifelse(x < (alpha/50), " ***", 
         ifelse(x < (alpha/5), " **", 
                ifelse(x < alpha, " *", 
                       ifelse(x < (alpha*2), " +", " n.s."))))
}


#' string position getter (like `base::substr()` but nicer interface + `collapse` option)
#' 
#' get the `idx`-th character in `string`
#'
#' @param string a character string
#' @param idx a position index
#' @param collapse whether to smoosh output together
#'
#' @return the `idx`-th character in `string`
#' @export
#'
#' @examples str_pos("boosh", 4)
str_pos <- function(string, idx=NULL, collapse=TRUE){
  # TODO: GENERALIZE TO WORK ON STRING VEXXE
  #       (note: quick fix w sapply doesnt work as just a wrapper)
  if (length(idx)==0){
    message(
      "yao, gotta gimme a index! (returning NULL)"
    )
    return(NULL)
    # ow if its just a number, get that char
  } else {
    if (length(idx)==1){
      chars <- strsplit(string, "")[[1]]
      return(chars[idx])
      # ow if its a non-trivial vector, split it and optionally collapse
    } else {
      # get all the chars as a vec
      chars <- unlist(strsplit(string, ""))
      # subset chars by index
      chars <- chars[idx]
      # if desire, paste them together and return; ow just return as a vec
      if (collapse==TRUE){
        return(paste(chars, collapse=""))
      } else {
        return(chars)
      }
    }
  }
}


#' initial segment of string(s) (similar to `base::substr()` but more intuitive)
#'
#' get the initial segment of length `nchar` for each element of `string_vec`
#' 
#' @param string_vec a vector of character strings 
#' @param nchar number of characters to return
#' @param keep_names for handling named vectors, `FALSE` by default
#'
#' @return initial segment of each string in the input vector
#' @export
#'
#' @examples init_seg(string_vec=c("hai,","my","name","is","timi!"), nchar=3) 
init_seg <- function(string_vec, nchar, keep_names=FALSE){
  out <- sapply(seq_along(string_vec), function(idx){
    if (is.na(string_vec[idx])){
      return(NA)
    } else {
      if (nchar(string_vec[idx]) < nchar){
        return(string_vec[idx])
      } else {
        return(paste(
          unlist(strsplit(string_vec[idx], ""))[seq_len(nchar)], 
          collapse=""
        ))
      }
    }
  })
  if (keep_names){
    return(out)
  } else {
    return(unname(out))
  }
}



#' final segment of string(s) (similar to `base::substr()` but more intuitive)
#'
#' get the final segment of length `nchar` for each element of `string_vec`
#' 
#' @param string_vec a vector of character strings 
#' @param nchar number of characters to return
#' @param keep_names for handling named vectors, `FALSE` by default
#'
#' @return final segment of each string in the input vector
#' @export
#'
#' @examples final_seg(string_vec=c("hai,","my","name","is","timi!"), nchar=3)
final_seg <- function(string_vec, nchar, keep_names=FALSE){
  out <- sapply(seq_along(string_vec), function(idx){
    if (is.na(string_vec[idx])){
      return(NA)
    } else {
      if (nchar(string_vec[idx]) < nchar){
        return(string_vec[idx])
      } else {
        return(
          paste(unlist(strsplit(string_vec[idx], ""))[
            (nchar(string_vec[idx])-nchar+1):nchar(string_vec[idx])
            ], collapse="")
        )
      }
    }
  })
  if (keep_names){
    return(out)
  } else {
    return(unname(out))
  }
}



#' convert to character quickly
#'
#' (just a wrapper around `as.character()`, for quick interactive + ad hoc use)
#' @param x an atomic vector
#'
#' @return `x` as a character string
#' @export
#'
#' @examples ac(12345)
ac <- function(x){
  as.character(x)
}


#' number of unique vals
#'
#' (just a wrapper around `length(unique())`, for quick interactive + ad hoc use)
#' 
#' @param x an atomic vector
#'
#' @return number of unique vals, as an integer
#' @export
#'
#' @examples lu(c(1,2,3,1,2,4))
lu <- function(x){
  length(unique(x))
}


#' round to two digits
#'
#' (just a wrapper for quick interactive + ad hoc use) 
#' 
#' @param x a numeric vector
#'
#' @return `x`, rounded to two digits
#' @export
#'
#' @examples r2(c(1.234, 2.34558))
r2 <- function(x){
  round(x, digits=2)
}


#' abbreviated summary of nested model comparison via LRT
#'
#' print just the test stat and p-value for a likelihood ratio test 
#' assessing relative fit between two nested models (computed w `anova()`)
#' 
#' @param mob_full a model object with k predictors
#' @param mob_red a model object with j predictors where j < k 
#' @param anova_object a comparison of nested models via `anova()`
#'
#' @return mostly for message side-effect, but returns 
#' the test stat and p-val in a named vector
#' @export 
#'
#' @examples 
#' fit <- lm(mpg ~ wt + cyl, data=mtcars)
#' fit_red <- lm(mpg ~ cyl, data=mtcars)
#' 
#' print_lrt_message(mob_full=fit, mob_red=fit_red)
#' 
#' comparison <- anova(fit, fit_red)
#' print_lrt_message(anova_object=comparison)
print_lrt_message <- function(mob_full=NULL, mob_red=NULL, anova_object=NULL){
  # throw error if it's not the case that exactly one of the following is true:
  #   - is.null(anova_object)
  #   - sum(is.null(mob_full), is.null(mob_red)) == 2
  stopifnot(xor(is.null(anova_object)), 
            sum(is.null(mob_full), is.null(mob_red)) == 2)
  
  if (is.null(anova_object)){
    message("computing LRT via `anova()` for `mob_full` against `mob_red`\n")
    anova_object <- anova(mob_full, mob_red)
  } 
  
  message(
    "\n------------------------------------------------------\n",
    "using likelihood-ratio test assuming null hypothesis:\n   ",
    "h_0: 'full model is no better than reduced model'\n\n",
    anova_object$Chisq[2], "    <~~ chi-square test stat from LRT",
    "\n", anova_object$`Pr(>Chisq)`[2], " <~~ p-value under the null",
    "\n------------------------------------------------------\n"
  )
  return(c(test_stat = anova_object$Chisq[2], 
           p_val = anova_object$`Pr(>Chisq)`[2]))
}


#' boot_sem
#' non-parametric bootstrap for standard error of the mean (or just boot mean)
#'
#' @param vec 
#' @param se logical, if `TRUE` (default), returns standard error of the mean; if `FALSE`, returns the mean of the bootstrap means 
#' @param b number of bootstrap resamples to draw 
#' @param n number of elements to draw during resampling (defaults to number of non-missing elements in input vector) -- setting `n=1` is jacknife resampling 
#'
#' @return `out`: booted se or mean of `vec`, depends on supplied `se` param val
#' @export
#'
#' @examples boot_se(rnorm(1000))
boot_sem <- function(vec, sem=TRUE, b=1000, 
                     n=sum(!is.na(vec)), na.rm=TRUE){
  # remove missings if desired 
  if (na.rm){ vec <- vec[!is.na(vec)] }
  
  # get the means of `b` bootstrap resamples of `vec`
  boot_means <- replicate(b, mean(sample(vec, size=n, replace=TRUE)))
  
  # return boot SEM (sd of boot means) if sem==TRUE; else return mean of means
  return(ifelse(sem==TRUE, sd(boot_means), mean(boot_means)))
}

