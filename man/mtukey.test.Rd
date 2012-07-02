\name{mtukey.test}
\alias{mtukey.test}
\title{Modified Tukey Additivity Test}
\description{
  Test for an interaction in two-way ANOVA table by the modified Tukey test.
}
\usage{
mtukey.test(Y, alpha=0.05, correction=0, Nboot=1000)
}
\arguments{
  \item{Y}{data matrix}
  \item{alpha}{level of the test}
  \item{correction}{type of small sample size correction (0=none, 1=bootstrap without replacement, 2=sampling), see \code{Details}}
  \item{Nboot}{number of simulations to be used for small sample size correction}
}
\details{
The level of the modified Tukey test is unstable for a small sample size. In such cases either bootstraping (\code{correction=1}) or sampling (\code{correction=2}) should be used to compute the critical value.
}
\references{
  Simecek, P. and Simeckova M.: On a Modification of The Tukey Additivity Test, \emph{To be appeared}. 
}
\seealso{
\code{\link{tukey.test}}, \code{\link{mandel.test}}, \code{\link{johnson.graybill.test}}, \code{\link{lbi.test}}, \code{\link{johnson.graybill.test}}
}
\examples{
  data(Boik)
  mtukey.test(Boik)
  mtukey.test(Boik,correction=2,Nboot=2000)
}
\keyword{htest}
