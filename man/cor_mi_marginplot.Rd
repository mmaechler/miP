\name{cor_mi_marginplot}
\alias{cor_mi_marginplot}
\title{MI Marginplots of all highly correlated variables}
\description{
  Computes correlations between all variables taking all observations
  without missing values on both variables.  Then plots MI Marginplots of
  all highly correlated variables.
}
\usage{
cor_mi_marginplot(input, cor = 0.5, obs = 0.5)
}
\arguments{
  \item{input}{MI object produced by mi, mice or Amelia.}
  \item{cor}{MI Marginplots between two variables are drawn only if
    their correlation is higher than the absolute value of this
    parameter.}
  \item{obs}{MI Marginplots between two variables are drawn only if the
    rate of observed values is higher than this parameter.}
}
\details{
  This plot is inspired by the \code{marginplot}  function in R package
  \CRANpkg{VIM}.
  For details see \code{\link[VIM]{marginplot}} there.
}
\value{
  Returns a correlation matrix, all correlations lower than abs(cor) are
  NA.
}
\references{
  R package "VIM: Visualization and Imputation of Missing Values"
  by Matthias Templ, Andreas Alfons, Alexander Kowarik.
  \url{cran.r-project.org/package=VIM}
}
\author{Paul Brix}

\seealso{
  \code{\link{mi_marginplot}} and  \code{\link[VIM]{marginplot}} from \CRANpkg{VIM}.
}
\examples{
data(FLAS)
require("mice")
FLASmi <- mice(FLAS)
cor_mi_marginplot(FLASmi)
}
\keyword{hplot}
\keyword{Multiple Imputation}

