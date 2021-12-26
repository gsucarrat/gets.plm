#################################################################
## Test file for gets.plm package. First created 18 January 2021,
## Oslo.
##
## 1 INITIATE
## 2 TEST gets.plm() WITH ARTIFICIAL DATA
## 3 TEST gets.plm() WITH REAL DATA
##
## Workflow: First estimate a model with plm(), then
## apply GETS to it. Example:
##
## mygum <- plm(y ~ x1 + x2 + x3)
## gets(mygum)
##
##
#################################################################


#################################################################
## 1 INITIATE
#################################################################

##set working directory:
setwd("C:/Users/sucarrat/Documents/R/gs/gets.plm/devel/")
#setwd(choose.dir())

##required packages:
library(gets)
library(plm)

##clean workspace:
rm(list = ls())

##load gets.plm source:
source("./gets.plm/R/gets.plm-source.R")


###########################################################
## 2 TEST gets.plm() WITH ARTIFICIAL DATA
###########################################################

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
  data=mydata) #effect="individual"
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=2) 
myspecific <- gets(mygum, t.pval=0.4)

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, effect="time")
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=2) 
myspecific <- gets(mygum, t.pval=0.4)

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, effect="twoways")
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=2)
myspecific <- gets(mygum, t.pval=0.4)

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, effect="nested")
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=2)
myspecific <- gets(mygum, t.pval=0.4)

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, model="random")
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

##note: 'plm' uses z-values instead of t-values for p-values

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=2)
myspecific <- gets(mygum, t.pval=0.4)

##new gum, do gets:
##=================

##for this to work, I have to find out how exogenous variables
##are constructed!

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, model="ht")
#summary(mygum)
#length(coef(mygum))
#dim(vcov(mygum))

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, model="between")
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=2)
myspecific <- gets(mygum, t.pval=0.4)

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, model="pooling")
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=2)
myspecific <- gets(mygum, t.pval=0.4)

##new gum, do gets:
##=================

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, model="fd")
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=2)
myspecific <- gets(mygum, t.pval=0.4)


###########################################################
## 3 TEST gets.plm() WITH REAL DATA
###########################################################

## basic example from help(plm) file
##==================================

data("Produc", package = "plm")
mygum <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=1)
myspecific <- gets(mygum, t.pval=0.4)


## based on example from help(plm) file:
## Hausman-Taylor estimator and Amemiya-MaCurdy estimator
## replicate Baltagi (2005, 2013), table 7.4
##=======================================================

data("Wages", package = "plm")
mygum <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
          bluecol + ind + union + sex + black + ed, 
          data = Wages, index = 595)
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
myspecific <- gets(mygum, keep=1)
myspecific <- gets(mygum, t.pval=0.4)

##the following works but is not correct, since the code is not
##capable of handling "|" in a correct manner yet
data("Wages", package = "plm")
mygum <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
          bluecol + ind + union + sex + black + ed |
          bluecol + south + smsa + ind + sex + black |
          wks + married + union + exp + I(exp ^ 2), 
          data = Wages, index = 595)
summary(mygum)
length(coef(mygum))
dim(vcov(mygum))

myspecific <- gets(mygum)
