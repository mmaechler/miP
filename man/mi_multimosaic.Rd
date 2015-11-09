\name{mi_multimosaic}
\alias{mi_multimosaic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multivariate Mosaicplot for imputed values
}
\description{
Draws mosaicplot for two or more categorical variables. Imputed values are highlighted in red.
}
\usage{
mi_multimosaic(x, input, nr = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of variablenames. Defines which variables should be plottet.
}
  \item{input}{
MI object produced by mi, mice.
}
  \item{nr}{
number of imputation chain that should be plottet.
}
}

\author{
Paul Brix
}


\seealso{
\link[vcd]{mosaic}
}
\examples{
data(FLAS)
FLASmi = mice(FLAS)
mi_multimosaic(c("GRD","SEX","AGE"),FLASmi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple Imputation}
