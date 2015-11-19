\name{FLAS}
\alias{FLAS}
\docType{data}
\title{Raymond's Foreign Language Attitude Scale (FLAS) Data}
\description{
  Raymond (1987) describes data that were collected to investigate the
  usefulness of a newly developed instrument, the Foreign Language
  Attitude Scale (FLAS), for predicting success in the study of foreign
  languages.  The dataset contains missing values.
}
\usage{data(FLAS)}
\format{
  A data frame with 279 observations on the following 12 variables.
  \describe{
    \item{\code{lang}}{foreign language studied, a \code{\link{factor}}
      with 4 levels ("french", "spanish", "german", "russian").}
    \item{\code{age}}{age group, a two-level \code{\link{factor}} with
      levels \code{"-19"} = less than 20, and  \code{"20+"} = 20 and more.}
    \item{\code{priC}}{the number of prior foreign language courses; as
      a 3-level factor w/ levels \code{c("none", "1--2", "3+")}.}
    \item{\code{sex}}{M=male, F=female.}
    \item{\code{FLAS}}{score on foreign language attitude scale.}
    \item{\code{MLAT}}{Modern Language Aptitude Test, fourth subtest score.}
    \item{\code{vSAT}}{verbal score of Scholastic Attitude Test.}
    \item{\code{mSAT}}{math   score of Scholastic Attitude Test.}
    \item{\code{eng}}{score on Penn State English placement exam.}
    \item{\code{HGPA}}{high scool grad point average.}
    \item{\code{CGPA}}{current college grad point average.}
    \item{\code{gradeB}}{final grade in foreign language course as
      binary (hence the \dQuote{B} in the variable name)
      factor w/ levels \code{"B-"} (B or lower), and \code{"A"}.}
  }
}
\details{
  This dataset is an adjusted version of the original dataset by Mark
  Raymond.  The adjustments can be looked up in Schafer (1997, p.200).
  Where Schafer called the variables \eqn{Y_1}, \eqn{Y_2}, \dots,
  \eqn{Y_{12}}, then in the original \pkg{miP} package they had
  all-capital \code{\link{colnames}}, see \code{FLAS.nms} in the example,
  these have been slightly modified by Martin Maechler; also, the 5
  \code{\link{factor}} variables got meaningful \code{\link{levels}}
  (instead of just integer codes \code{1}, \code{2}, \dots).
}
\references{
  Schafer, J.L. (1997)
  \emph{Analysis of Incomplete Multivariate Data}
  Chapman and Hall, London.
}
\examples{
data(FLAS)
str(FLAS)
summary(FLAS) # summary *includes* number of NA's

### The original (2011 `miP` CRAN package) data "labeling" and coding:

##  1) labeling all factors with integers; this is exactly
which(iF <- sapply(FLAS, is.factor)) # 1  2  3  4  12
FLAS.n <- FLAS
FLAS.n[iF] <- lapply(FLAS[iF], function(.) as.factor(as.integer(.)))
##  2) and using all capital variable names:
FLAS.nms <- c("LAN",  "AGE",  "PRI", "SEX",  "FLAS", "MLAT",
              "SATV", "SATM", "ENG", "HGPA", "CGPA", "GRD")
names(FLAS.n) <- FLAS.nms
## -> FLAS.n  is what 'FLAS' used to be in 2011's miP version

}
\keyword{datasets}
