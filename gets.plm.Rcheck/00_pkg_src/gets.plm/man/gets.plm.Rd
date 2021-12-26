\name{gets.plm}
\alias{gets.plm}

\title{General-to-Specific (GETS) modelling of models of class 'plm' (linear paneldata models)}

\description{
  General-to-Specific (GETS) modelling of models of class 'plm' (linear paneldata models), see \code{\link{plm}}.
}

\usage{
  \method{gets}{plm}(x, t.pval = 0.05, wald.pval = t.pval, do.pet = TRUE,
    keep = NULL, include.gum = FALSE, include.1cut = TRUE,
    include.empty = FALSE, max.paths = NULL, turbo = TRUE, tol = 1e-07,
    print.searchinfo = TRUE, alarm = FALSE, ...)
}

\arguments{
  \item{x}{an object of class 'plm'}
  \item{t.pval}{\code{numeric} value between 0 and 1. The significance level used for the two-sided regressor significance t-tests }
  \item{wald.pval}{\code{numeric} value between 0 and 1. The significance level used for the Parsimonious Encompassing Tests (PETs) }
  \item{do.pet}{\code{logical}. If \code{TRUE}, then a Parsimonious Encompassing Test (PET) against the GUM is undertaken at each regressor removal for the joint significance of all the deleted regressors along the current path. If \code{FALSE}, then a PET is not undertaken at each regressor removal}
  \item{keep}{\code{NULL} or an integer vector that indicates which regressors to be excluded from removal in the search}
  \item{include.gum}{\code{logical}. If \code{TRUE}, then the GUM (i.e. the starting model) is included among the terminal models. If \code{FALSE}, then the GUM is not included}
  \item{include.1cut}{\code{logical}. If \code{TRUE}, then the 1-cut model is added to the list of terminal models. If \code{FALSE}, then the 1-cut is not added, unless it is a terminal model in one of the paths }
  \item{include.empty}{\code{logical}. If \code{TRUE}, then the empty model is added to the list of terminal models. If \code{FALSE}, then the empty model is not added, unless it is a terminal model in one of the paths }
  \item{max.paths}{\code{NULL} or an integer greater than 0. If \code{NULL}, then there is no limit to the number of paths. If an integer (e.g. 1), then this integer constitutes the maximum number of paths searched (e.g. a single path) }
  \item{turbo}{\code{logical}. If \code{TRUE}, then (parts of) paths are not searched twice (or more) unnecessarily, thus yielding a significant potential for speed-gain. However, the checking of whether the search has arrived at a point it has already been comes with a slight computational overhead. Accordingly, if \code{turbo=TRUE}, then the total search time might in fact be higher than if \code{turbo=FALSE}. This happens if estimation is very fast, say, less than quarter of a second }
  \item{tol}{numeric value. The tolerance for detecting linear dependencies in the columns of the variance-covariance matrix when computing the Wald-statistic used in the Parsimonious Encompassing Tests (PETs), see the \code{\link{qr.solve}} function}
  \item{print.searchinfo}{\code{logical}. If \code{TRUE}, then a print is returned whenever simiplification along a new path is started}
  \item{alarm}{\code{logical}. If \code{TRUE}, then a sound or beep is emitted (in order to alert the user) when the model selection ends }
  \item{\dots}{additional arguments (currently ignored) }
}

\value{
  an object of class \code{\link{plm}}
}

\author{
Genaro Sucarrat, \url{http://www.sucarrat.net/}
}

\seealso{
  \code{\link{getsFun}}, \code{\link{plm}}
}

\examples{

##create some artificial data:
##============================

iN <- 20 #no. of firms
iT <- 4 #no. of time periods (e.g. year)
iNiT <- iN*iT
set.seed(123)
Z <- rnorm(iNiT)
x <- matrix(rnorm(iNiT*10), iNiT, 10)
colnames(x) <- letters[1:10]
firm <- as.vector( t( 1:iN*matrix(rep(1,iNiT), iN, iT) ) )
year <- rep(2001:2004, iN)
mydata <- data.frame(firm, year, Z, x)
head(mydata)

##delete unnecessary stuff from workspace:
rm(iN, iT, iNiT, Z, x, firm, year)

##estimate gum, do gets:
##======================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata)
summary(mygum)

myspecific <- gets(mygum)

myspecific <- gets(mygum, keep=2)

myspecific <- gets(mygum, t.pval=0.4)

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, effect="twoways")
summary(mygum)

myspecific <- gets(mygum)

myspecific <- gets(mygum, keep=2)

myspecific <- gets(mygum, t.pval=0.4)

}


\keyword{Statistical Models}
\keyword{Time Series}
\keyword{Econometrics}
\keyword{Climate Econometrics}
\keyword{Financial Econometrics}
\keyword{Panel Data}

