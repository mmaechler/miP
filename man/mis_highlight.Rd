\name{mis_highlight}
\alias{mis_highlight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Highlight missing values
}
\description{
Draws histograms, spinograms or boxplots of all variables and highlights all observed values for all observations that have missing values for the variable defined by parameter var.
}
\usage{
mis_highlight(ds, var, n = "all", type = "hist")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
data.frame.
}
  \item{var}{
name of variable that should be highlighted.
}
  \item{n}{
vector of variablenames which are to be plotted. Default: "all".
}
  \item{type}{
"hist" plots histograms, "spin" spinograms, "box" boxplots.
}
}
\author{
Paul Brix
}
\examples{
data(FLAS)
mis_highlight(FLAS,"MLAT")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Missing Data}
