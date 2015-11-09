\name{mi_multimosaic}
\title{Multivariate Mosaicplot for imputed values}
\alias{mi_multimosaic}
\description{
  Draws mosaicplot for two or more categorical variables.  Imputed values
  are highlighted in red.
}
\usage{
mi_multimosaic(x, input, nr = 1)
}
\arguments{
  \item{x}{vector of variable names determining which variables should be plotted.}
  \item{input}{MI object produced by mi, mice.}
  \item{nr}{number of imputation chain that should be plotted.}
}

\author{Paul Brix}

\seealso{
  \code{\link[vcd]{mosaic}} in package \pkg{vcd}.
}
\examples{
data(FLAS)
require("mice")
FLASmi <- mice(FLAS)
mi_multimosaic(c("grade","sex","age"), FLASmi)
}
\keyword{hplot}
\keyword{Multiple Imputation}
