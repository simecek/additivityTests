\name{johnson.graybill.test}
\alias{johnson.graybill.test}
\title{Johnson and Graybill Additivity Test}
\usage{
johnson.graybill.test(Y, alpha = 0.05, critical.value = NA, Nsim = 1000)
}
\arguments{
  \item{Y}{data matrix}

  \item{alpha}{level of the test}

  \item{critical.value}{result of
  \code{\link{critical.values}} function, see
  \code{Details}}

  \item{Nsim}{number of simulations to be used for a
  critical value estimation}
}
\value{
A list with class "\code{aTest}" containing the following
components: test statistics \code{stat}, critical value
\code{critical.value} and the result of the test
\code{result}, i.e. whether the additivity hypothesis has
been rejected.
}
\description{
Test for an interaction in two-way ANOVA table by the
Johnson-Graybill test.
}
\details{
The critical value can be computed in advance and given in
the parameter \code{critical value}. If not a function
\code{\link{critical.values}} is called to do that.
}
\examples{
data(Boik)
johnson.graybill.test(Boik)
}
\references{
Johnson, D.E. and Graybill, F.A.: An analysis of a two-way
model with interaction and no replication, \emph{Journal of
the American Statistical Association} \bold{67}, pp.
862--868, 1972.
}
\seealso{
\code{\link{tukey.test}}, \code{\link{mtukey.test}},
\code{\link{mandel.test}}, \code{\link{lbi.test}},
\code{\link{tusell.test}}
}
\keyword{htest}

