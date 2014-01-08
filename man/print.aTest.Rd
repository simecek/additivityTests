\name{print.aTest}
\alias{print.aTest}
\title{Print aTest object}
\usage{
  \method{print}{aTest} (x,
    digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{x}{aTest object}

  \item{how}{many significant digits are to be used}
}
\value{
  \code{NULL}
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

