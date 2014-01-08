\name{print.aTest}
\alias{print.aTest}
\title{Print aTest object}
\usage{
  print.aTest(x, ...)
}
\arguments{
  \item{x}{aTest object}
}
\description{
  Print the results of additivity test.
}
\examples{
data(Boik)
t <- tukey.test(Boik)
print(t)
}
\keyword{internal}

