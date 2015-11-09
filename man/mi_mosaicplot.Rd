\name{mi_mosaicplot}
\title{Mosaicplot for Imputed Values}
\alias{mi_mosaicplot}
\description{
  Draws a mosaicplot for all categorical variables.  The plot is divided
  into observed and imputed values.
}
\usage{
mi_mosaicplot(input, n="all", chains = FALSE)
}
\arguments{
  \item{input}{MI object produced by mi, mice.}
  \item{n}{vector of variablenames. Defines which variables are to be
    plotted, default: "all".}
  \item{chains}{whether to plot each imputation chain for itself or not.}
}
\author{Paul Brix}

\examples{
data(FLAS)
require("mice")
FLASmi <- mice(FLAS)
mi_mosaicplot(FLASmi)
}
\keyword{hplot}
\keyword{Multiple Imputation}
