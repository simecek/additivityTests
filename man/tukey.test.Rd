\name{tukey.test}
\alias{tukey.test}
\title{Tukey Additivity Test}
\usage{
tukey.test(data, alpha = 0.05, critical.value = NA)
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
Test for an interaction in two-way ANOVA table by the Tukey
test.
}
\details{
The critical value can be computed in advance and given in
the parameter \code{critical value}. If not a function
\code{\link{critical.values}} is called to do that.
}
\examples{
data(Boik)
tukey.test(Boik)
}
\references{
Tukey, J.W.: One Degree of Freedom for Non-additivity,
\emph{Biometrics} \bold{5}, pp. 232--242, 1949.
}
\seealso{
\code{\link{tusell.test}}, \code{\link{mtukey.test}},
\code{\link{mandel.test}}, \code{\link{lbi.test}},
\code{\link{johnson.graybill.test}}
}
\keyword{htest}

