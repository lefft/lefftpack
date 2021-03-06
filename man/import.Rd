% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{import}
\alias{import}
\title{python-style import}
\usage{
import(what, from, msg = TRUE)
}
\arguments{
\item{what}{function to be imported (*not quoted*)}

\item{from}{path to file that defines `what` (character)}

\item{msg}{whether to print a message on success ("func X was imported"); defaults to `TRUE`}
}
\value{
no return value
}
\description{
import a single function from a file (instead of getting them all with `source()`)
}
\examples{
# usage example (assumes `boosh()` is defined in "util.r")
import(what=boosh, from="util.r") # `boosh()` gets created in calling env
rm(boosh) # clear `boosh()` from ws
blaowwie <- function(){
  import(boosh, from="util.r"); boosh("blah1","blah2")}
blaowwie() # `boosh()` will *not* be created in calling env of `blaowwie()` 
}
