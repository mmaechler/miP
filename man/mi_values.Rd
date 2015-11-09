\name{mi_values}
\alias{mi_values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots to visualize imputed values
}
\description{
Draws boxplots, densityplots or histograms of the observed (blue) and imputed (red) values.
}
\usage{
mi_values(input, type = "boxplot1", var = "all", mis_count = 5,
 m_hist = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
MI object produced by mi, mice or Amelia.
}
  \item{type}{
"hist" draws histograms of a specific imputation chain. "boxplot1" / "densityplot1" plots the observed values and all imputed values for every imputation chain. "boxplot2" / "densityplot2" plots the observed values and all imputed values without distinguishing between the different imputation chains. "boxplot3" / "densityplot3" plots the observed values and all values from the completed imputed dataset for each imputation chain.
}
  \item{var}{
vector that defines which variables are to be plotted, default is "all".
}
  \item{mis_count}{
densityplots are only drawn for every variable with more than mis_count missing values.
}
  \item{m_hist}{
number of the imputation chain that should be plotted. Only if type = "hist".
}
}

\author{
Paul Brix
}
\examples{
data(FLAS)
FLASmi = mice(FLAS)
mi_values(FLASmi, type="hist")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple Imputation}
