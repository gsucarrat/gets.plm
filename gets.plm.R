#iN <- 20 #no. of firms
#iT <- 4 #no. of time periods (e.g. year)
#iNiT <- iN*iT
#set.seed(123)
#Z <- rnorm(iNiT)
#x <- matrix(rnorm(iNiT*10), iNiT, 10)
#colnames(x) <- letters[1:10]
#firm <- as.vector( t( 1:iN*matrix(rep(1,iNiT), iN, iT) ) )
#year <- rep(2001:2004, iN)
#mydata <- data.frame(firm, year, Z, x)
#head(mydata)
#
###delete unnecessary stuff from workspace:
#rm(iN, iT, iNiT, Z, x, firm, year)
#
###
#x <- 
#  plm(Z ~ a + b + c + d + e + f + g + h + i + j,
##  data=mydata, model="within") #effect="individual"
#  data=mydata, model="random")
##  data=mydata, model="between")
#summary(x)

###
#data("Produc", package = "plm")
#x <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#          data = Produc, index = c("state","year"))
#summary(x)

##
#data("Wages", package = "plm")
#x <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
#          bluecol + ind + union + sex + black + ed, 
#          data = Wages, index = 595)
#summary(x)

##
data("Wages", package = "plm")
x <- plm(lwage ~ wks + south + smsa + married + exp + I(exp ^ 2) + 
          bluecol + ind + union + sex + black + ed |
          bluecol + south + smsa + ind + sex + black |
          wks + married + union + exp + I(exp ^ 2), 
          data = Wages, index = 595)
summary(x)

##arguments:
#x
t.pval=0.05 #default: 0.05
wald.pval=t.pval #default: t.pval
do.pet=TRUE #default: TRUE
keep=NULL #default: NULL
include.gum=FALSE #default: FALSE
include.1cut=TRUE #default: TRUE
include.empty=FALSE #default: FALSE
max.paths=NULL #default: NULL
turbo=TRUE #default: TRUE
tol=1e-07 #default: 1e-07
print.searchinfo=TRUE #default: TRUE
alarm=FALSE #default: FALSE
    
##begin code:
#{
