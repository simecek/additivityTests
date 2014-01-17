\name{mtukey.test}
\alias{mtukey.test}
\title{Modified Tukey Additivity Test}
\usage{
mtukey.test(Y, alpha = 0.05, correction = 0, Nboot = 1000)
}
\arguments{
  \item{Y}{data matrix}

  \item{alpha}{level of the test}

  \item{correction}{type of small sample size correction
  (0=none, 1=bootstrap without replacement, 2=sampling),
  see \code{Details}}

  \item{Nboot}{number of simulations to be used for small
  sample size correction}
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
modified Tukey test.
}
\details{
The level of the modified Tukey test is unstable for a
small sample size. In such cases either bootstraping
(\code{correction=1}) or sampling (\code{correction=2})
should be used to compute the critical value.
}
\examples{
data(Boik)
mtukey.test(Boik)
mtukey.test(Boik,correction=2,Nboot=2000)
}
\references{
Simecek, Petr, and Simeckova, Marie. "Modification of
Tukey's additivity test." \emph{Journal of Statistical
Planning and Inference}, \bold{2012}.
}
\seealso{
\code{\link{tukey.test}}, \code{\link{mandel.test}},
\code{\link{johnson.graybill.test}},
\code{\link{lbi.test}}, \code{\link{johnson.graybill.test}}
}
\keyword{htest}

