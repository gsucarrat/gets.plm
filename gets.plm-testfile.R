#################################################################
## Test file for gets.plm package. First created 18 January 2021,
## Oslo.
##
## 1 INITIATE
## 2 CREATE ARTIFICIAL DATA
## 3 TEST gets.plm()
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
setwd("C:/Users/sucarrat/Documents/R/gs/gets.plm/github/")
#setwd(choose.dir())

##required packages:
library(gets)
library(plm)

##clean workspace:
rm(list = ls())

##load gets.plm source:
source("./gets.plm/R/gets.plm.R")


###########################################################
## 2 CREATE ARTIFICIAL DATA
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

##check that plm works on data:
##=============================

myplm <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata)
summary(myplm)


###########################################################
## 3 TEST gets.plm()
###########################################################

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

##new gum, do gets:
##=================

##these do not work, because the number of coefficients is
##increased by one (intercept is added); this is not accommodated
##in the current code

mygum <- 
  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
  data=mydata, model="random")
summary(mygum)

myspecific <- gets(mygum) #101 estimations
summary(myspecific)

myspecific <- gets(mygum, turbo=TRUE) #56 estimations
summary(myspecific)

myspecific <- gets(mygum, keep=2)
summary(myspecific)

myspecific <- gets(mygum, t.pval=0.4)
summary(myspecific)
