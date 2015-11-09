\name{mi_marginplot}
\alias{mi_marginplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Marginplot including imputed values
}
\description{
In addition to a standard scatterplot, information about missing and imputed values is shown in the plot margins.
}
\usage{
mi_marginplot(x, input, leg=TRUE, col = c("skyblue","red","red4"), 
           alpha = NULL, pch = c(1,16), cex = par("cex"), 
           numbers = TRUE, cex.numbers = par("cex"), 
           zeros = FALSE, xlim = NULL, ylim = NULL, 
           main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
           ann = par("ann"), axes = TRUE, frame.plot = axes, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of two variablenames.
}
  \item{input}{
MI object produced by mi, mice or Amelia.
}
\item{leg}{
draw a legend, TRUE or FALSE.
}

\item{col}{a vector of length three defining the colors to be used in the plot. The first color is used for the scatterplot and the boxplots for the available data, the second color for the univariate scatterplots and boxplots for the missing values in one variable, and the third color for the frequency of missing values in both variables (see 'Details'). If only one color is supplied, it is used for the bivariate and univariate scatterplots and the boxplots for missing values in one variable, whereas the boxplots for the available data are transparent. Else if two colors are supplied, the second one is recycled.
} 
\item{alpha}{a numeric value between 0 and 1 giving the level of transparency of the colors, or NULL. This can be used to prevent overplotting.
} 
\item{pch}{a vector of length two giving the plot symbols to be used for the scatterplot and the univariate scatterplots. If a single plot character is supplied, it is used for the scatterplot and the default value will be used for the univariate scatterplots (see 'Details').
} 
\item{cex}{the character expansion factor to be used for the bivariate and univariate scatterplots.
} 
\item{numbers}{a logical indicating whether the frequencies of missing values should be displayed in the lower left of the plot (see 'Details').
} 
\item{cex.numbers}{the character expansion factor to be used for the frequencies of the missing values.
} 
\item{zeros}{a logical vector of length two indicating whether the variables are semi-continuous, i.e., contain a considerable amount of zeros. If TRUE, only the non-zero observations are used for drawing the respective boxplot. If a single logical is supplied, it is recycled.
} 
\item{xlim, ylim}{axis limits.
} 
\item{main, sub}{main and sub title.
} 
\item{xlab, ylab}{axis labels.
} 
\item{ann}{a logical indicating whether plot annotation (main, sub, xlab, ylab) should be displayed.
} 
\item{axes}{a logical indicating whether both axes should be drawn on the plot. Use graphical parameter "xaxt" or "yaxt" to suppress only one of the axes.
} 
\item{frame.plot}{a logical indicating whether a box should be drawn around the plot.
} 
\item{\dots}{
graphical parameters, see \link[graphics]{par}.
}
}
\details{
Modified version of the marginplot which is included in the VIM package. Thanks to Matthias Templ, Andreas Alfons for letting me use their source code.

Boxplots for available, missing and imputed data, as well as scatterplots of the observed values. Missing values in one variable are shown in the plot margins.

Furthermore, the frequencies of the missing values can be displayed by a number (lower left of the plot). The number in the lower left corner is the number of observations that are missing in both variables.
}
\references{
R package "VIM: Visualization and Imputation of Missing Values" 
by Matthias Templ, Andreas Alfons, Alexander Kowarik.
\url{cran.r-project.org/package=VIM}
}
\author{
Original code by Andreas Alfons and Matthias Templ, modifications by Paul Brix
}
\seealso{
\link[VIM]{marginplot}
}
\examples{
data(FLAS)
FLASmi = mice(FLAS)
mi_marginplot(c("MLAT","ENG"),FLASmi,pch=16)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Multiple Imputation}
\keyword{Marginplot}
