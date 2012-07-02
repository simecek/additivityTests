\name{tukey.test}
\alias{tukey.test}
\title{Tukey Additivity Test}
\description{
  Test for an interaction in two-way ANOVA table by the Tukey test.
}
\usage{
tukey.test(data, alpha = 0.05, critical.value = NA)
}
\arguments{
  \item{data}{data matrix}
  \item{alpha}{level of the test}
  \item{critical.value}{NA = default}
}
\value{
A list with class "\code{aTest}" containing the following components: test statistics \code{stat}, critical value \code{critical.value} and the result of the test \code{result}, i.e. whether the additivity hypothesis has been rejected.
}
\references{Tukey, J.W.: One Degree of Freedom for Non-additivity, \emph{Biometrics} \bold{5}, pp. 232--242, 1949.}
\examples{
  data(Boik)
  tukey.test(Boik)
}
\keyword{htest}
