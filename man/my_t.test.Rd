% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_t.test.R
\name{my_t.test}
\alias{my_t.test}
\title{my t.test for data}
\usage{
my_t.test(x, alternative = "two.sided", mu = 60)
}
\arguments{
\item{x}{Numeric vector input}

\item{alternative}{String input, should be \code{"two.sided"}, \code{"less"} or \code{"greater"}.}

\item{mu}{Numeric input, the mean of the null hypothesis value. default to \code{60}.}
}
\value{
A list includes alternative and p_value.
}
\description{
my t.test for data
}
\examples{
my_t.test(x)
my_t.test(x, alternative="less")
my_t.test(x, alternative="greater", mu=60)

}
\keyword{inference}
