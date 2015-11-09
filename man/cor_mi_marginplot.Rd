\name{cor_mi_marginplot}
\alias{cor_mi_marginplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MI Marginplots of all highly correlated variables
}
\description{
Computes correlations between all variables taking all observations without missing values on both variables. Then plots MI Marginplots of all highly correlated variables.
}
\usage{
cor_mi_marginplot(input, cor = 0.5, obs = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
MI object produced by mi, mice or Amelia.
}
  \item{cor}{
MI Marginplots between two variables are drawn only if their correlation is higher than the absolute value of this parameter.
}
  \item{obs}{
MI Marginplots between two variables are drawn only if the rate of observed values is higher than this parameter.
}
}
\details{This plot is inspired by the marginplot available in the R package VIM. For details see \link[VIM]{marginplot}.
}
\value{
Returns a correlation matrix, all correlations lower than abs(cor) are NA.}
\references{
R package "VIM: Visualization and Imputation of Missing Values" 
by Matthias Templ, Andreas Alfons, Alexander Kowarik.
\url{cran.r-project.org/package=VIM}
}
\author{
Paul Brix
}


\seealso{
\link[miP]{mi_marginplot}, 
\link[VIM]{marginplot}
}
\examples{
data(FLAS)
FLASmi = mice(FLAS)  
cor_mi_marginplot(FLASmi)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple Imputation}
\keyword{Marginplot}

