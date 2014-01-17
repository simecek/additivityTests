\name{critical.values}
\alias{critical.values}
\title{Critical Values for the Johnson-Graybill, LBI and Tusell tests}
\usage{
critical.values(a, b, N = 1e+05, alpha = 0.05)
}
\arguments{
  \item{a}{number of rows}

  \item{b}{number of columns}

  \item{N}{number of simulations}

  \item{alpha}{level(s) of the test}
}
\value{
A list containing three components: critical values for
Johnson-Graybill, LBI and Tusell tests, respectively.
}
\description{
Compute the critical values by performing N simulation.
}
\examples{
data(Boik)
critical.values(nrow(Boik), ncol(Boik), 0.01)
}
\seealso{
\code{\link{johnson.graybill.test}},
\code{\link{lbi.test}}, \code{\link{tusell.test}}
}
\keyword{htest}

