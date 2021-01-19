\name{gets.plm}
\alias{gets.plm}

\title{General-to-Specific (GETS) modelling of models of class 'plm' (linear paneldata models)}

\description{
  General-to-Specific (GETS) modelling of models of class 'plm' (linear paneldata models), see \code{\link{plm}}.
}

\usage{
  \method{gets}{plm}(x, ...)
}

\arguments{
  \item{x}{an object of class 'plm'}
  \item{\dots}{additional arguments passed on to \code{\link{getsFun}} }
}

\value{
  \item{gets:}{an object of class 'plm', see \code{\link{plm}} }
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

myspecific <- gets(mygum) #101 estimations
summary(myspecific)

myspecific <- gets(mygum, turbo=TRUE) #56 estimations
summary(myspecific)

myspecific <- gets(mygum, keep=2)
summary(myspecific)

myspecific <- gets(mygum, t.pval=0.4)
summary(myspecific)

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, effect="twoways")
summary(mygum)

myspecific <- gets(mygum) #101 estimations
summary(myspecific)

myspecific <- gets(mygum, turbo=TRUE) #56 estimations
summary(myspecific)

myspecific <- gets(mygum, keep=2)
summary(myspecific)

myspecific <- gets(mygum, t.pval=0.4)
summary(myspecific)

}


\keyword{Statistical Models}
\keyword{Time Series}
\keyword{Econometrics}
\keyword{Climate Econometrics}
\keyword{Financial Econometrics}
