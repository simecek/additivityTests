\name{mandel.test}
\alias{mandel.test}
\title{Mandel Additivity Test}
\usage{
  mandel.test(data, alpha = 0.05, critical.value = NA)
}
\arguments{
  \item{data}{data matrix}

  \item{alpha}{level of the test}

  \item{critical.value}{result of
  \code{\link{critical.values}} function, see
  \code{Details}}
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
  Mandel test.
}
\details{
  The critical value can be computed in advance and given
  in the parameter \code{critical value}. If not a function
  \code{\link{critical.values}} is called to do that.
}
\examples{
data(Boik)
mandel.test(Boik)
}
\references{
  Mandel, J.: Non-additivity in Two-way Analysis of
  Variance, \emph{Journal of the American Statistical
  Association} \bold{56}, pp. 878--888, 1961.
}
\seealso{
  \code{\link{tukey.test}}, \code{\link{mtukey.test}},
  \code{\link{johnson.graybill.test}},
  \code{\link{lbi.test}}, \code{\link{tussel.test}}
}
\keyword{htest}

