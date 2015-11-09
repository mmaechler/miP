\name{obs_plot}
\title{Plot of observed values}
\alias{obs_plot}
\description{
  Scatterplot, boxplot or doubledeckerplot (depending on the variable
  types) between a specific variable and all other variables from a
  dataset. Only observations that have no missing values for both
  variables are plotted. Fitted curves are drawn in red.
}
\usage{
obs_plot(ds, var, main = "default")
}
\arguments{
  \item{ds}{data.frame.}
  \item{var}{plots are drawn between the variable defined by this
    parameter and all other variables from the dataset.}
  \item{main}{defines the title of the plots. Default the percentage of
    observed values for both variables is plotted.}
}
\details{Draws scatterplots between two continuous variables, boxplots
  between categorical and continious and doubledeckerplots between two
  categorical variables.
}
\author{Paul Brix}
\examples{
data(FLAS)
obs_plot(FLAS,"MLAT")
}
\keyword{hplot}
\keyword{Missing Data}
