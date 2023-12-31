% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize the data grouped by years & months}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{Numeric (including vector) for the years you want to check}
}
\value{
A summarised tibble data frame spread out by years
}
\description{
Summary of the data from \code{\link[=fars_read_years]{fars_read_years()}}, uses tidyverse packages to process the data
}
\examples{
\dontrun{dat <- fars_summarize_years(c(2013, 2014, 2015))}
}
\seealso{
\code{\link[=fars_read_years]{fars_read_years()}}
}
