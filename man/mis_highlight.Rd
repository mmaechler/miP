\name{mis_highlight}
\title{Highlight Missing Valuesa}
\alias{mis_highlight}
\description{
  Draws histograms, spinograms or boxplots of all variables and
  highlights all observed values for all observations that have missing
  values for the variable defined by parameter var.
}
\usage{
mis_highlight(ds, var, n = "all", type = "hist")
}
\arguments{
  \item{ds}{data.frame.}
  \item{var}{name of variable that should be highlighted.}
  \item{n}{vector of variablenames which are to be plotted. Default: "all".}
  \item{type}{"hist" plots histograms, "spin" spinograms, "box" boxplots.}
}

\author{Paul Brix}
\examples{
data(FLAS)
mis_highlight(FLAS,"MLAT")
}
\keyword{hplot}
\keyword{Missing Data}
