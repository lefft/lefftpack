### functions for data analysis (updated jun21/2017)

## TODO list: 
# 
#   - write documentation for new foncs at bottom
#       > quiet_attach()
#       > boot_se()
# 
## wishlist: 
# 
#   - center a variable around zero
#   - bootstrap regression coefficients
#   - simple permutation test func
#   - more general perm func
#   - ggplot2 theme(s?)
#   - want to represent `colz()` as data not fonc
#   - ...


# calculate 95% confidence intervals via non-parametric bootstrap 
#' bootstrap mean and other summary stats
#'
#' @param vec 
#' @param b 
#' @param n 
#' @param dig 
#' @param narm 
#'
#' @return val 
#' @export
#'
#' @examples boosh 
bootCI <- function(vec, b=1000, n=length(vec[!is.na(vec)]), dig=6, narm=TRUE){
  # can remove missings (build in logic for missings later)
  vec <- vec[!is.na(vec)]
  # make a container for results
  boots <- rep(NA, times=b) 
  # conduct bootstrap resampling
  for (x in 1:b){
    boots[x] <- mean(sample(vec, size=n, replace=TRUE), na.rm=narm)
  }
  # boot mean is mean of the boots
  bootMean <- mean(boots, na.rm=narm)
  # boot std error is sd of the boot means
  bootSE <- sd(boots, na.rm=narm)
  # boot ci's are +/-2 boot se's
  bootLo <- mean(vec, na.rm=narm) - 1.96*bootSE
  bootHi <- mean(vec, na.rm=narm) + 1.96*bootSE
  # collect results + return them
  return(round(c(
    "bootMean"=bootMean,
    "sampMean"=mean(vec),
    "bootSE"=bootSE,
    "sampSD"=sd(vec),
    "bootLo"=bootLo,
    "bootHi"=bootHi
  ), digits=dig))
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
  round(sqrt((p*(1-p))/n), digits=2)
}

# reasonable color palette for plotting
#' colorze
#'
#' @return val 
#' @export
#'
#' @examples boosh 
colz <- function(){
  c(lite_tan="#dbd4d0", claudy_braun="#afa69e", afghan_tan="#c8ab94",
    baygish="#68442a", bilbao="#248100", japlore="#2F742C")
}


# (equiv to a wrapper around `scale()` w defaults, but returns atomic vec)
#' quick zscore func 
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



### simple + commonly used utility functions

# attach packages w/o annoying messages
#' quiet_attach
#'
#' @param packages 
#' @param shhh 
#'
#' @return val 
#' @export
#'
#' @examples boosh 
quiet_attach <- function(packages, shhh=TRUE){
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


# annotate pvals w stars
#' pvalClassify
#'
#' @param x 
#' @param alpha 
#'
#' @return val 
#' @export
#'
#' @examples boosh 
pvalClassify <- function(x, alpha=.05){
  ifelse(x < (alpha/50), " (***)", 
         ifelse(x < (alpha/5), " (**)", 
                ifelse(x < alpha, " (*)", 
                       ifelse(x < (alpha*2), " +", " n.s."))))
}


#' string position getter
#'
#' @param string a character string
#' @param idx a position index
#'
#' @return the `idx`-th character in `string`
#' @export
#'
#' @examples str_pos("boosh", 4)
str_pos <- function(string, idx){
  chars <- strsplit(string, "")[[1]]
  return(chars[idx])
}


#' convert to character quickly
#'
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
#' @param x a numeric vector
#'
#' @return `x`, rounded to two digits
#' @export
#'
#' @examples r2(c(1.234, 2.34558))
r2 <- function(x){
  round(x, digits=2)
}


# print summary of the results of a lrt test, from an anova object
print_lrt_message <- function(anova_object){
  message(
    "\n------------------------------------------------------\n",
    "using likelihood-ratio test assuming null hypothesis:\n   ",
    "h_0: 'interaction model is no better than the model w/o interaction'\n\n",
    round(anova_object$Chisq[2], 2), "     <~~ chi-square test stat from LRT",
    "\n", round(anova_object$`Pr(>Chisq)`[2], 8), "  <~~ p-value under the null",
    "\n------------------------------------------------------\n"
  )
}

# bootstrap mean and standard error of the mean
#' boot_se
#'
#' @param vec 
#' @param se 
#' @param b 
#' @param n 
#' @param dig 
#'
#' @return `out`: booted se or mean of `vec`, depends on supplied `se` param val
#' @export
#'
#' @examples boot_se(1:10)
boot_se <- function(vec, se=TRUE, b=1000, n=length(vec[!is.na(vec)]), dig=2){
  # remove missings (build in logic for missings later)
  vec <- vec[!is.na(vec)]
  # make a container for results
  boots <- rep(NA, times=b)
  # conduct bootstrap resampling
  for (x in 1:b){
    boots[x] <- mean(sample(vec, size=n, replace=TRUE))
  }
  # return:   bootstrap se    if se=TRUE;
  #           bootstrap mean  if se=FALSE
  out <- ifelse(se==TRUE, sd(boots), mean(boots))
  return(out)
}



