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

##CHALLENGE: To obtain the data used with model.matrix(),
##model.frame() and the formula used, since there does not
##seem to be a consistent way that works across models and
##specifications

###########################################################
## gets.plm()
###########################################################
  
gets.plm <- function(x, t.pval=0.05, wald.pval=t.pval, do.pet=TRUE,
  keep=NULL, include.gum=FALSE, include.1cut=TRUE, include.empty=FALSE,
  max.paths=NULL, turbo=TRUE, tol=1e-07, print.searchinfo=TRUE, 
  alarm=FALSE, ...)
{
  ## 1 initialise
  ## 2 create gum data
  ## 3 define user-specified estimator
  ## 4 estimate and print gum (start model)
  ## 5 do gets
  ## 6 estimate final model
  ## 7 return result

  ##---------------
  ## 1 initialise
  ##---------------

  ##call arguments:
  callAsList <- as.list(x$call)
  listOfArgs <- callAsList[-1]
  whereIndex <- which( names(listOfArgs)=="index" )
  if( length(whereIndex)>0 ){
    ##full index is obtained below and added to GUMdata
    listOfArgs[ whereIndex ] <- NULL
    ##note: "index" argument should probably be added back
    ##if final model is estimated.
  }

  ##stop if model=="between":
  if( !is.null(listOfArgs$model) ){
    if( listOfArgs$model %in% c("between", "fd") ){
      stop( "GETS modelling not available (yet?) for this model")
    }
  }

  ##coefs, vcovs, df for empty model:
  coefs <- coef(x) #for the check of myEstimator()
  vcovs <- vcov(x) #for the check of myEstimator()
  lengthCoefs <- length(coefs)
  coefHasIntercept <-
    ifelse( names(coefs)[1]=="(Intercept)", TRUE, FALSE)
  if( coefHasIntercept ){
    lengthCoefs <- lengthCoefs - 1
    coefs <- coefs[-1]
    vcovs <- cbind(vcovs[,-1])
    vcovs <- rbind(vcovs[-1,])
  }
  dfs0 <- df.residual(x) + lengthCoefs #df for empty model:

  ##is the start model empty?:
  if( lengthCoefs < 1 ){ stop("no variables to search over") }

  ##pipe part in formula?
  formulatxt <- as.character(x$formula)[3]
  wherePipe <- integer(0)
  for(i in 1:nchar(formulatxt)){
    if( substr(formulatxt,i,i) == "|"){ wherePipe <- i; break }      
  }
  if( length(wherePipe) > 0 ){
    stop("pipe(s) '|' in formula not possible yet")
#for the future:
#    pipetxt <- substr(formulatxt, wherePipe,nchar(formulatxt))
#    pipetxt <- paste0(" ", pipetxt)
  }else{
    pipetxt <- character(0)
  }

  ##-------------------
  ## 2 create gum data
  ##-------------------
    
  ##obtain data:
  idAndTime <- index(x)
  modelFrame <- model.frame(x)
  frameNames <- colnames(modelFrame)
  
  ##create y and x:
  y <- modelFrame[,1]
  yName <- frameNames[1]
  xNamesOriginal <- names(coefs)
  x <- model.matrix(x)
  x <- as.data.frame(x[,xNamesOriginal])
  xNames <- paste0("x", 1:NCOL(x))
  colnames(x) <- xNames
  attr(x, "contrasts") <- NULL
  attr(x, "assign") <- NULL

  ##create gum data:
  GUMdata <- cbind(idAndTime, y, x)
  
  ##re-define y and x:
  y <- c("y", "y")
  x <- xNames
  x <- rbind(x,x)
  colnames(x) <- paste0("x", 1:NCOL(x))

  ##do some clean-up (to reduce memory usage):
  rm(idAndTime, modelFrame)


  ##-----------------------------
  ## 3 user-specified estimator
  ##-----------------------------
  
  myEstimator <- function(y, x, data=NULL, listOfArgs=NULL)
  {
    ##handle NULL-matrices (obligatory):
    if(is.null(x) || NCOL(x)==0){
  
      result <- list()
    	eps <- GUMdata[,y[1]]
#    	eps <- data[,y[1]]
    	result$logl <- sum(dnorm(eps, sd=sd(eps), log=TRUE))
    	result$n <- NROW(eps)
    	result$k <- 0
    	result$df <- dfs0
  
    }else{ ##if x is not NULL:
  
      ##make formula:
      myformula <- paste0(x[1,], collapse=" + ")
      myformula <- paste0("y ~ ", myformula)
#      myformula <- paste0(y[1], " ~ ", x[1,1])
#      if( NCOL(x)>1 ){
#        for(i in 2:NCOL(x)){
#          myformula <- paste0(myformula, " + ", x[1,i])
#        }
#      }
      myformula <- paste0(myformula, pipetxt)
      myformula <- as.formula(myformula)

      ##estimate:
      listOfArgs$formula <- myformula
      listOfArgs$data <- GUMdata
#      listOfArgs$data <- data
      tmp <- do.call("plm", listOfArgs)
  
    	#rename and re-organise:
     	result <- list()
      coefs <- coef(tmp)
      vcovs <- vcov(tmp)
      coefsHasIntercept <- 
        ifelse( names(coefs)[1]=="(Intercept)", TRUE, FALSE)
      if( coefsHasIntercept ){ 
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
  
    }
  
    ##final output:
    return(result)

  } #close myEstimator()


  ##----------------------------------------
  ## 4 estimate and print gum (start model)
  ##----------------------------------------

  ##estimate gum using myEstimator():
  gum.result <-
    myEstimator(y, x, data=GUMdata, listOfArgs=listOfArgs)

  ##check the gum produced by myEstimator():
  ##----------------------------------------
  
  ##do checks
  gumOK <- rep(NA,3)
  gumOK[1] <- length(coefs) == length(gum.result$coefficients)
  gumOK[2] <-
    all(round(coefs, digits=11) == round(gum.result$coefficients, digits=11))
  gumOK[3] <-
    all(round(vcovs, digits=11) == round(gum.result$vcov, digits=11))
  
  ##warning?:
  if( all(gumOK)==FALSE ){
    warning("gum (start model) of 'myEstimator' not identical to gum of 'plm'")
  }

  ##print the gum:
  ##--------------  

  ##ensure gum object exists:
  gum <- NULL 
  
  if( print.searchinfo ){

    coefs <- gum.result$coefficients  
    stderrs <- sqrt(diag(gum.result$vcov))
  
    ##create gum matrix:
    tstats <- coefs/stderrs
    colnamesgum <-
      c("reg.no", "keep", "coef", "std.error", "t-stat", "p-value")
    gum <- matrix(NA, length(coefs), length(colnamesgum))
    colnames(gum) <- colnamesgum
    rownames(gum) <- xNamesOriginal
#    rownames(gum) <- names(coefs)
        
    ##fill contents:
    gum[,"coef"] <- coefs
    gum[,"std.error"] <- stderrs
    gum[,"t-stat"] <- tstats
    gum[,"p-value"] <- ##same as in getsFun():
      pt(abs(tstats), df=gum.result$df, lower.tail=FALSE)*2
    gum[,"reg.no"] <- 1:length(coefs)
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
    
  ##------------
  ## 5 do gets
  ##------------

  getsResult <- getsFun(y, x,
    user.estimator=list(name="myEstimator", data=GUMdata,
    listOfArgs=listOfArgs, envir=environment()),
    t.pval=t.pval, wald.pval=t.pval, do.pet=do.pet, keep=keep,
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
      cat(paste0("  ", xNamesOriginal[as.numeric(getsResult$specific.spec)]), "\n")
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


  ##----------------
  ## 6 final model
  ##----------------

#for the future: try to estimate final model
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
  ## 7 return result
  ##------------------
  
  result <- getsResult
  return(result)
  
} #close gets.plm
