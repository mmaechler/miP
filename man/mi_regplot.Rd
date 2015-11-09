\name{mi_regplot}
\title{Scatterplot - Observed/Imputed vs Predicted}
\alias{mi_regplot}
\description{
  Bivariate scatterplot that plots the observed or imputed values for each observation
  against the predicted values, overlaid with fitted lowess curves.
}
\usage{
mi_regplot(input, nr = 1, nmis=5)
}
\arguments{
  \item{input}{MI object produced by mi, mice.}
  \item{nr}{number of the imputation chain that should be plotted.}
  \item{nmis}{lowess curve is only plotted if number of imputed values is
    higher than this parameter.}
}
\details{
  The plot works only for continuous variables and for objects produced by
  mi or mice (method="norm"), not Amelia.
}
\author{Paul Brix}
\examples{
data(FLAS)
require("mice")
FLASmi <- mice(FLAS, defaultMethod = c("norm","logreg","polyreg"))
mi_regplot(FLASmi)
}
\keyword{hplot}
\keyword{Multiple Imputation}
