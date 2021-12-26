###########################################################
## This file contains the source code of the gets.plm
## package.
##
## First created 8 July 2020 by Genaro Sucarrat.
##
## CONTENTS:
##
## gets.plm()
##                       
###########################################################

##IDEAS:
## - use paste0(xnames, collapse=" + ") to efficiently
##   create formulas

###########################################################
## gets.plm()
###########################################################
  
gets.plm <- function(x, t.pval=0.05, wald.pval=t.pval, do.pet=TRUE,
  keep=NULL, include.gum=FALSE, include.1cut=TRUE, include.empty=FALSE,
  max.paths=NULL, turbo=TRUE, tol=1e-07, print.searchinfo=TRUE, 
  alarm=FALSE, ...)
{
  ## 1 initialise
  ## 2 print start model (the gum)
  ## 3 user-specified estimator
  ## 4 do gets
  ## 5 return result

  ##---------------
  ## 1 initialise
  ##---------------

  ##call arguments:
  callAsList <- as.list(x$call)
  listOfArgs <- callAsList[-1]
  whereIndex <- which( names(listOfArgs)=="index" )
  if( length(whereIndex)>0 ){ listOfArgs[ whereIndex ] <- NULL }
  
  ##obtain data:
  modelFrame <- model.frame(x)
  xSformula <- x$formula
  modelMatrix <- #"Formula" class:
    model.matrix(xSformula, data=modelFrame)
#OLD (based on "formula" class):
#  modelMatrix <- model.matrix(as.formula(listOfArgs$formula),
#    data=modelFrame)
  idAndTime <- index(x)
      
  ##regressor names, standard errors, dfs:
  coefs <- coef(x)
  coefNames <- names(coefs)
  xNames <- coefNames  
  stderrs <- sqrt(diag(vcov(x)))
  dfs <- df.residual(x)
  dfs0 <- dfs + length(xNames)
    
  ##is there an intercept?
  hasIntercept <- ifelse(coefNames[1]=="(Intercept)", TRUE, FALSE)
  if( hasIntercept ){
    xNames <- xNames[-1]
    dfs0 <- dfs0 - 1
  }    

  ##startmodel empty?:
  if( length(xNames)==0 ){ stop("no variables to search over") }
  
  ##create gum data:
  yName <- colnames(modelFrame)[1]
  y <- modelFrame[,yName]
  x <- as.data.frame(modelMatrix[,xNames])
  colnames(x) <- paste0("x", 1:NCOL(x))
  GUMdata <- as.data.frame(cbind(idAndTime, y, x))

  ##re-define x:
  x <- colnames(x)
  x <- rbind(x,x)

  ##create pipe part (if any):
  ##(these are ideas primarily for the future)
  formulatxt <- as.character(xSformula)[3]
  wherePipes <- integer(0)
  for(i in 1:nchar(formulatxt)){
    tmp <- substr(formulatxt,i,i)
    if( tmp == "|"){ wherePipes <- c(wherePipes, i) }      
  }
  if( length(wherePipes) > 0 ){
    stop("pipes | in formula not possible (yet)")
#    pos <- c(wherePipes, nchar(formulatxt))
#    form <- paste0(yName, " ~", substr(formulatxt, pos[1]+1, pos[2]-1),
#        " - 1")
#    z <- as.data.frame(model.matrix(as.formula(form), modelFrame))
  }

  ##do some clean-up:
  rm(idAndTime, modelFrame, modelMatrix)
    
  ##------------------------------
  ## 2 print start model (the gum)
  ##------------------------------
  
  gum <- NULL #make sure the 'gum' exists
  if( print.searchinfo ){
  
    ##create gum matrix:
    tstats <- coefs/stderrs
#OLD:
#    dfs <- length(y) - length(coefs)
    colnamesgum <-
      c("reg.no", "keep", "coef", "std.error", "t-stat", "p-value")
    gum <- matrix(NA, length(coefs), length(colnamesgum))
    colnames(gum) <- colnamesgum
    rownames(gum) <- names(coefs)
        
    ##fill contents:
    gum[,"coef"] <- coefs
    gum[,"std.error"] <- stderrs
    gum[,"t-stat"] <- tstats
    gum[,"p-value"] <- 2*pt(abs(tstats), df=dfs, lower.tail=FALSE)
    if( hasIntercept ){ gum <- rbind(gum[-1,]) }
    gum[,"reg.no"] <- 1:length(xNames)
    gum[,"keep"] <- 0
    if( !is.null(keep) ){ gum[keep,"keep"] <- 1}
    gum <- as.data.frame(gum)
    
    ##print gum:
    cat("\n")
    cat("Dependent variable:", yName, "\n")
    cat("\n")
    cat("Start model (GUM):\n")
    cat("\n")
    printCoefmat(gum, cs.ind=c(3,4), tst.ind=c(5), signif.stars=TRUE,
      P.values=TRUE)
    cat("\n")

  } #end if( print.searchinfo )
  
  ##-----------------------------
  ## 3 user-specified estimator
  ##-----------------------------
  
  myEstimator <- function(y, x, data=NULL, listOfArgs=NULL)
  {
    ##handle NULL-matrices (obligatory):
    if(is.null(x) || NCOL(x)==0){
  
      result <- list()
    	eps <- data[,"y"]
    	result$logl <- sum(dnorm(eps, sd=sd(eps), log=TRUE))
    	result$n <- NROW(eps)
    	result$k <- 0
    	result$df <- dfs0
#OLD:
#    	result$df <- result$n - result$k
  
    }else{ ##if x is not NULL:
  
      ##make formula:
      myformula <- paste0("y ~ ", x[1,1])
      if(NCOL(x)>1){
        for(i in 2:NCOL(x)){
          myformula <- paste0(myformula, " + ", x[1,i])
        }
      }
      myformula <- as.formula(myformula)

      ##estimate:
      listOfArgs$formula <- myformula
      listOfArgs$data <- GUMdata #should be data?
      tmp <- do.call("plm", listOfArgs)
  
    	#rename and re-organise:
     	result <- list()
      coefs <- coef(tmp)
      vcovs <- vcov(tmp)
      if( hasIntercept ){ #hasIntercept should be an argument in myEstimator?
        coefs <- coefs[-1]
        vcovs <- cbind(vcovs[,-1])
        vcovs <- rbind(vcovs[-1,])
      }
      result$coefficients <- coefs
      result$vcov <- vcovs
    	eps <- residuals(tmp)
    	result$logl <- sum(dnorm(eps, sd=sd(eps), log=TRUE))
    	result$n <- NROW(eps)
    	result$k <- NCOL(x)
    	result$df <- df.residual(tmp)
#OLD:
#    	result$df <- result$n - result$k
  
    }
  
    ##final output:
    return(result)

  } #close myEstimator()

  ##------------
  ## 4 do gets
  ##------------

  getsResult <- getsFun(y, x,
    user.estimator=list(name="myEstimator", data=GUMdata,
    listOfArgs=listOfArgs, envir=environment()), t.pval=t.pval,
    wald.pval=t.pval, do.pet=do.pet, keep=keep,
    include.gum=include.gum, include.1cut=include.1cut,
    include.empty=include.empty, max.paths=max.paths, turbo=turbo,
    tol=tol, print.searchinfo=print.searchinfo, alarm=alarm)
  bestTerminal <- getsResult$terminals[[ getsResult$best.terminal ]]
  getsResult$call <- NULL
  getsResult$gum <- gum

  ##print paths, terminals and retained regressors:
  if( print.searchinfo && !is.null(getsResult$terminals.results) ){

    ##paths:
    if( length(getsResult$paths)>0 ){
      cat("\n")
      for(i in 1:length(getsResult$paths)){
        txt <- paste0(getsResult$paths[[i]], collapse=" ")
        txt <- paste0("  Path ", i, ": ", txt)    
        cat(txt, "\n")
      }
    }

    ##terminals:
    cat("\n")
    cat("Terminal models:\n")
    cat("\n")
    print(getsResult$terminals.results)    
      
    ##retained regressors:
    cat("\n")
    cat("Retained regressor(s):\n")
    cat("\n")
    if( length(getsResult$specific.spec)==0 ){
      cat("  none\n")
    }else{
      cat(paste0("  ", xNames[as.numeric(getsResult$specific.spec)]), "\n")
    }

  } #end if( print.searchinfo )

  ##messages:
  if( print.searchinfo && !is.null(getsResult$messages) ){
    cat("\n")
    cat("Messages:\n")
    cat("\n")
    cat(getsResult$messages)
    cat("\n")
  }

#for the future?: try to estimate final model
#
#  ##----------------
#  ## 5 final model
#  ##----------------
#
#  if( estimate.final ){ then...etc. }
#  plmResult <- NULL
#  plmClass <- NULL
#
#  ##if non-empty (estimate final model):
#  if( length(bestTerminal)>0 ){
#
#    ##create formula:
#    bestXs <- xTerms[bestTerminal]    
#    myformula <- paste0(yTerm, " ~ ", bestXs[1])
#    if( length(bestTerminal)>1 ){
#      for(i in 2:length(bestTerminal)){
#        myformula <- paste0(myformula, " + ", bestXs[i])
#      }
#    }
##OLD:
##    bestXs <- xNames[bestTerminal]    
##    myformula <- paste0(yName, " ~ ", bestXs[1])
##    if( length(bestTerminal)>1 ){
##      for(i in 2:length(bestTerminal)){
##        myformula <- paste0(myformula, " + ", bestXs[i])
##      }
##    }
#    myformula <- as.formula(myformula)
#  
#    ##estimate final model:
#    listOfArgs$formula <- myformula
#    plmResult <- do.call("plm", listOfArgs)
#
#    ##extract class:
#    plmClass <- class(plmResult)
#  }
#  
#  result <- c(getsResult, plmResult)
#  if(!is.null(plmClass)){ class(result) <- plmClass }

  ##------------------
  ## 5 return result
  ##------------------
  
  result <- getsResult
  return(result)
  
} #close gets.plm
