\name{mi_regplot}
\alias{mi_regplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scatterplot - Observed/Imputed vs Predicted}
\description{
Bivariate scatterplot that plots the observed or imputed values for each observation
against the predicted values, overlaid with fitted lowess curves.
}
\usage{
mi_regplot(input, nr = 1, nmis=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
MI object produced by mi, mice.
}
  \item{nr}{
number of the imputation chain that should be plotted.
}
\item{nmis}{
lowess curve is only plotted if number of imputed values is higher than this parameter.
}
}
\details{
Plot works only for continuous variables and for objects produced by mi or mice (method="norm"), not Amelia.
}
\author{
Paul Brix
}
\examples{
data(FLAS)
FLASmi = mice(FLAS,defaultMethod=c("norm","logreg","polyreg"))
mi_regplot(FLASmi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple Imputation}
