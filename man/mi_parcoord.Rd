\name{mi_parcoord}
\alias{mi_parcoord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallel Coordinates Plot for imputed values
}
\description{
Draws parallel coordinates plot for a imputed dataset. Highlightes all imputed observations of a specific variable in red. All other observations are drawn in grey.
}
\usage{
mi_parcoord(input, var, n = "all", nr = 1,col="rainbow")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
MI object produced by mi, mice or Amelia.
}
  \item{var}{
defines variable that should be highlighted.
}
  \item{n}{
vector of variables that should be plottet, default = "all".
}
  \item{nr}{
number of the imputation chain that should be plottet or vector if more than one imputation chain should be plotted.
}
 \item{col}{
defines in which colors the several imputation chains should be highlighted. Only arises if the parameter nr is a vector.
}
}

\author{
Paul Brix
}

\seealso{
\link[MASS]{parcoord}
}


\examples{
data(FLAS)
FLASmi = mice(FLAS)
mi_parcoord(FLASmi,"MLAT",n=c("FLAS","SATV","SATM","ENG"))
}

\keyword{Multiple Imputation}
