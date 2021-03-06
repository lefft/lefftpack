% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{str_pos}
\alias{str_pos}
\title{string position getter (like `base::substr()` but nicer interface + `collapse` option)}
\usage{
str_pos(string, idx = NULL, collapse = TRUE)
}
\arguments{
\item{string}{a character string}

\item{idx}{a position index}

\item{collapse}{whether to smoosh output together}
}
\value{
the `idx`-th character in `string`
}
\description{
get the `idx`-th character in `string`
}
\examples{
str_pos("boosh", 4)
}
