\name{mis_overview}
\alias{mis_overview}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Overview of missing values
}
\description{
Equal-sized barcharts of all variables. Green indicates observed values, red missing values.
}
\usage{
mis_overview(ds, srt = 0, pos = -3, percent = FALSE, inc = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
data.frame.
}
  \item{srt}{
graphical parameter that adjusts the rotation of the variablenames.
}
  \item{pos}{
graphical parameter that adjusts the position of the count of missing values. 
}
  \item{percent}{
defines whether or not to draw the absolute count of missing values or the percentage.
}
  \item{inc}{
defines whether or not to calculate the number of observations that are complete for all variables.
}
}
\details{Additionally, an absolute value or percentage of missing values is drawn inside of every barchart.
}
\value{
Returns how many values are missing in total and how many observations are complete for all variables (very time consuming for big datasets).
}
\author{
Paul Brix
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(FLAS)
mis_overview(FLAS,srt=45)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Missing Data}
