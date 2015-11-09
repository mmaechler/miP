\name{mi_categories}
\title{Visualize Imputed Categorical Values}
\alias{mi_categories}
\description{
  Plot to visualize which categories have been imputed by the
  different imputation chains for a certain categorical variable.
}
\usage{
mi_categories(input, var, sort = FALSE, legend = FALSE)
}
\arguments{
  \item{input}{MI object produced by \pkg{mi}% function ?? FIXME
    or \code{\link[mice]{mice}()} from \pkg{mice}.}
  \item{var}{variablename of a \code{\link{factor}} to be plotted.}
  \item{sort}{defines whether or not to sort by categories.}
  \item{legend}{logical specifying if a legend should be drawn.}
}

\details{y-axis represents the m different imputation chains, x-axis all
  imputed observations. Each colour stands for one category.
  Therefore each box illustrates which category has been imputed for each
  missing value differed by imputation chains.
}
\author{Paul Brix}
\examples{
data(FLAS)
require("mice")
FLASmi <- mice(FLAS)
mi_categories(FLASmi, "priC", sort=TRUE)
}
\keyword{hplot}
\keyword{Multiple Imputation}
