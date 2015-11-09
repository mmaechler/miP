\name{mi_categories}
\alias{mi_categories}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize imputed categorical values
}
\description{Plot to visualize which categories have been imputed by the different imputation chains for a certain categorical variable.
}
\usage{
mi_categories(input, var, sort = FALSE, legend = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{MI object produced by mi, mice.
}
  \item{var}{
variablename to be plotted, class(var) has to be "factor".
}
  \item{sort}{
defines whether or not to sort by categories.
}
  \item{legend}{
draw legend, TRUE or FALSE.
}
}
\details{y-axis represents the m different imputation chains, x-axis all imputed observations. Each colour stands for one category.
Therefore each box illustrates which category has been imputed for each missing value differed by imputation chains.
}
\author{
Paul Brix
}
\examples{
data(FLAS)
FLASmi = mice(FLAS)
mi_categories(FLASmi,"PRI",sort=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple Imputation}
