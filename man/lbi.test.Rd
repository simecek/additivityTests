\name{lbi.test}
\alias{lbi.test}
\title{Locally Best Invariant (LBI) Additivity Test}
\usage{
lbi.test(Y, alpha = 0.05, critical.value = NA, Nsim = 1000)
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
Test for an interaction in two-way ANOVA table by the LBI
test.
}
\details{
The critical value can be computed in advance and given in
the parameter \code{critical value}. If not a function
\code{\link{critical.values}} is called to do that.
}
\examples{
data(Boik)
lbi.test(Boik)
}
\references{
Boik, R.J.: Testing additivity in two-way classifications
with no replications:the locally best invariant test,
\emph{Journal of Applied Statistics} \bold{20},pp. 41--55,
1993.
}
\seealso{
\code{\link{tukey.test}}, \code{\link{mtukey.test}},
\code{\link{mandel.test}},
\code{\link{johnson.graybill.test}},
\code{\link{tusell.test}}
}
\keyword{htest}

