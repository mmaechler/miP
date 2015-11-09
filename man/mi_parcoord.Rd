\name{mi_parcoord}
\title{Parallel Coordinates Plot for imputed values}
\alias{mi_parcoord}
\description{
  Draws parallel coordinates plot for a imputed dataset.  Highlights all
  imputed observations of a specific variable in red.  All other
  observations are drawn in grey.
}
\usage{
mi_parcoord(input, var, n = "all", nr = 1, col = "rainbow")
}
\arguments{
  \item{input}{MI object produced by one of the\pkg{mi}, \pkg{mice} or
    \pkg{Amelia} \R packages.}
  \item{var}{defines variable that should be highlighted.}
  \item{n}{vector of variable names to plot; by default \code{"all"}.}
  \item{nr}{the number of the imputation chain to plot or a
    vector if more than one imputation chain should be plotted.}
  \item{col}{defines in which colors the several imputation chains should
    be highlighted.  Only relevant if \code{nr} is a vector.}
}

\author{Paul Brix}

\seealso{
  \code{\link[MASS]{parcoord}()} from package \CRANpkg{MASS}.
}

\examples{
data(FLAS)
require("mice")
FLASmi <- mice(FLAS)
mi_parcoord(FLASmi, var = "MLAT", n = c("FLAS","vSAT","mSAT","eng"))
}
\keyword{hplot}
\keyword{Multiple Imputation}
