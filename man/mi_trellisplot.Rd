\name{mi_trellisplot}
\alias{mi_trellisplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trellis Scatterplot for imputed datasets.
}
\description{
Draws Trellis Scatterplots for imputed datasets. Imputed values are drawn in red, observed in blue.
}
\usage{
mi_trellisplot(x, var, input, nr = 1, eqnr = 3, ol = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of variablenames that should be plotted on x- and y-axis.
}
  \item{var}{ defines the variable that is splitted continuously in equal intervals or a categorical variable.

}
  \item{input}{
MI object produced by mi, mice or Amelia.
}
  \item{nr}{
number of the imputation chain that should be plotted.
}
  \item{eqnr}{number of intervals var should be splitted into.

}
  \item{ol}{defines the overlap rate between the intervalls.

}
}

\author{
Paul Brix
}

\seealso{
\link[lattice]{xyplot}
}
\examples{
data(FLAS)
FLASmi = mice(FLAS)
mi_trellisplot(x=c("FLAS","SATV"),var="ENG",FLASmi,eqnr=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple Imputation}
