\name{tussel.test}
\alias{tussel.test}
\title{Tussel Additivity Test}
\usage{
  tussel.test(Y, alpha = 0.05, critical.value = NA,
    Nsim = 1000)
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
  Tussel test.
}
\details{
  The critical value can be computed in advance and given
  in the parameter \code{critical value}. If not a function
  \code{\link{critical.values}} is called to do that.
}
\examples{
data(Boik)
tussel.test(Boik)
}
\references{
  Tusell, F.: Testing for Interaction in Two-way ANOVA
  Tables with no Replication, \emph{Computational
  Statistics \& Data Analysis} \bold{10}, pp. 29--45, 1990
}
\seealso{
  \code{\link{tukey.test}}, \code{\link{mtukey.test}},
  \code{\link{mandel.test}}, \code{\link{lbi.test}},
  \code{\link{johnson.graybill.test}}
}
\keyword{htest}

