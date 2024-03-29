% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isName.R
\name{isName}
\alias{isName}
\title{isName}
\usage{
isName(txt)
}
\arguments{
\item{txt}{character: name(s) to check}
}
\value{
a logical vector
}
\description{
Checks if a \code{txt} consists of valid name(s):
\itemize{
\item A name must start with a letter and can be a combination of letters, digits, period(.) and underscore(_).
\item Reserved words cannot be used as a name (TRUE, FALSE, NULL, if...)
}
}
\examples{
isName("?plot")
isName(".default")
}
