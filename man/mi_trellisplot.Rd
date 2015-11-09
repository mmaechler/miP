\name{mi_trellisplot}
\title{Trellis Conditional Scatterplot for Imputed Datasets}
\alias{mi_trellisplot}
\description{
  Draws Trellis conditional scatterplots for imputed datasets.  Imputed
  values by default are drawn in red, observed in blue.
}
\usage{
mi_trellisplot(x, var, input, nr = 1, eqnr = 3, ol = 0.1,
             main = NULL,
             col = c("blue","red"), pch = 16, font.main = 2, ...)
}
\arguments{
  \item{x}{vector of variablenames that should be plotted on x- and y-axis.}
  \item{var}{defines the variable that is splitted continuously in
    equal intervals or a categorical variable.}
  \item{input}{MI object produced by mi, mice or Amelia.}
  \item{nr}{number of the imputation chain that should be plotted.}
  \item{eqnr}{number of intervals var should be splitted into.}
  \item{ol}{defines the overlap rate between the intervalls.}
  %% new 2015-11:
  \item{main}{main title; by default the conditioning formula.}
  \item{col}{vector of length two, specifying the colors for observed
    and imputed values, respectively.}
  \item{pch, font.main}{plot attributes, passed to \code{xyplot()}.}
  \item{\dots}{potentially further arguments passed to
  \code{\link[lattice]{xyplot}()}.}
}

\author{Paul Brix; tweaks by Martin Maechler}

\seealso{
  \code{\link[lattice]{xyplot}} from package \pkg{lattice}
}
\examples{
data(FLAS)
require("mice")
FLASmi <- mice(FLAS)
mi_trellisplot(x=c("FLAS","vSAT"), var="eng", FLASmi, eqnr=5)
}
\keyword{hplot}
\keyword{Multiple Imputation}
