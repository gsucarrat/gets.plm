pkgname <- "gets.plm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('gets.plm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("gets.plm")
### * gets.plm

flush(stderr()); flush(stdout())

### Name: gets.plm
### Title: General-to-Specific (GETS) modelling of models of class 'plm'
###   (linear paneldata models)
### Aliases: gets.plm
### Keywords: Statistical Models Time Series Econometrics Climate
###   Econometrics Financial Econometrics Panel Data

### ** Examples


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




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
