% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{ac}
\alias{ac}
\title{convert to character quickly}
\usage{
ac(x)
}
\arguments{
\item{x}{an atomic vector}
}
\value{
`x` as a character string
}
\description{
(just a wrapper around `as.character()`, for quick interactive + ad hoc use)
}
\examples{
ac(12345)
}
