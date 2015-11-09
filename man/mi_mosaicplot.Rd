\name{mi_mosaicplot}
\alias{mi_mosaicplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mosaicplot for imputed values
}
\description{
Draws a mosaicplot for all categorical variables. The plot is divided into observed and imputed values.
}
\usage{
mi_mosaicplot(input, n="all", chains = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
MI object produced by mi, mice.
}
  \item{n}{
vector of variablenames. Defines which variables are to be plotted, default: "all".
}
  \item{chains}{
whether to plot each imputation chain for itself or not.
}
}

\author{
Paul Brix
}


\examples{
data(FLAS)
FLASmi = mice(FLAS)
mi_mosaicplot(FLASmi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple Imputation}
