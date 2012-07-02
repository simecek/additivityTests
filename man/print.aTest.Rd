\name{print.aTest}
\alias{print.aTest}
\title{Print aTest object}
\description{Print the results of additivity test.}
\usage{
print.aTest(x, ...)
}
\arguments{
  \item{x}{aTest object}
}
\examples{
  data(Boik)
  t<-tukey.test(Boik)
  print(t) 
}
\seealso{
\code{\link{tukey.test}}, \code{\link{mtukey.test}}, \code{\link{mandel.test}}, \code{\link{johnson.graybill.test}}, \code{\link{lbi.test}}, \code{\link{tussel.test}}
}
\keyword{internal}
