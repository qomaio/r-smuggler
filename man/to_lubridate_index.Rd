% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smuggler.R
\name{to_lubridate_index}
\alias{to_lubridate_index}
\title{Create a lubridate index}
\usage{
to_lubridate_index(rng)
}
\arguments{
\item{rng}{FAME range}
}
\value{
tibble with lubridate date column
}
\description{
Create a lubridate index
}
\examples{
rng <- to_fame_range(rhli::HANDEC,"1993","2002")
tbl <- to_lubridate_index(rng)

}
