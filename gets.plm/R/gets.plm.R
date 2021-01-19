#################################################################
## This file contains the source code of the gets.plm package.
##
## First created 8 July 2020 by Genaro Sucarrat.
##
## CONTENTS:
##
## gets.plm()
##                       
#################################################################


###########################################################
## 3 PROTOTYPE OF gets.plm()
###########################################################
  
gets.plm <- function(x, ...)
{

  callAsList <- as.list(x$call)
  listOfArgs <- callAsList[-1]
  plmdata <- as.data.frame( get(as.character(callAsList$data)) )
  idAndTime <- plmdata[,1:2]
  
  ##extract names:
  formulaNames <- all.vars(callAsList$formula)
  yName <- formulaNames[1]
  xNames <- formulaNames[-1]
  
  ##make copies of y and x:
  y <- plmdata[,yName]
  x <- plmdata[,xNames]
  colnames(x) <- paste0("x", 1:NCOL(x))
  
  ##create GUMdata myEstimator:
  GUMdata <- as.data.frame(cbind(idAndTime, y, x))
  
  ##do some clean-up:
  rm(idAndTime)
  
  ##re-define x:
  x <- colnames(x)
  x <- rbind(x,x)
  
  ##create user-specified estimator:
  ##================================
  
  myEstimator <- function(y, x, data=NULL, listOfArgs=NULL)
  {
    ##handle NULL-matrices (obligatory):
    if(is.null(x) || NCOL(x)==0){
  
      result <- list()
    	eps <- data[,"y"]
    	result$logl <- sum(dnorm(eps, sd=sd(eps), log=TRUE))
    	result$n <- NROW(eps)
    	result$k <- 0
    	result$df <- result$n - result$k
  
    }else{ ##if x is not NULL:
  
      ##make formula:
      myformula <- paste0("y ~ ", x[1,1])
#OLD:
#      myformula <- paste0("y ~ ", x[1,1])
      if(NCOL(x)>1){
        for(i in 2:NCOL(x)){
          myformula <- paste0(myformula, " + ", x[1,i])
        }
      }
      myformula <- as.formula(myformula)

      ##estimate:
      listOfArgs$formula <- myformula
      listOfArgs$data <- GUMdata
      tmp <- do.call("plm", listOfArgs)
  
    	#rename and re-organise:
     	result <- list()
    	result$coefficients <- coef(tmp)
    	result$vcov <- vcov(tmp)
    	eps <- residuals(tmp)
    	result$logl <- sum(dnorm(eps, sd=sd(eps), log=TRUE))
    	result$n <- NROW(eps)
    	result$k <- NCOL(x)
    	result$df <- result$n - result$k
  
    }
  
    ##final output:
    return(result)

  } #close myEstimator()

  ##do gets:
  ##========

  ##do the gets:
  getsResult <- getsFun(y, x,
    user.estimator=list(name="myEstimator", data=GUMdata,
    listOfArgs=listOfArgs, envir=environment()), ...)
##for testing purposes:
##    listOfArgs=listOfArgs, envir=environment()), keep=5)
  bestTerminal <- getsResult$terminals[[ getsResult$best.terminal ]]
  getsResult$call <- NULL

  ##final model:
  ##============
  
  plmResult <- NULL
  plmClass <- NULL
  
  ##if empty:
  if( length(bestTerminal)==0 ){ cat("\n   Final model is empty \n\n") }
  
  ##if non-empty (estimate final model):
  if( length(bestTerminal)>0 ){

    ##create formula:
    bestXs <- xNames[bestTerminal]    
    myformula <- paste0(yName, " ~ ", bestXs[1])
#OLD:
#    myformula <- paste0(yName, " ~ ", bestXs)
    if( length(bestTerminal)>1 ){
      for(i in 2:length(bestTerminal)){
        myformula <- paste0(myformula, " + ", bestXs[i])
      }
    }
    myformula <- as.formula(myformula)
  
    ##estimate final model:
    listOfArgs$formula <- myformula
    plmResult <- do.call("plm", listOfArgs)

    ##extract class:
    plmClass <- class(plmResult)
  }
  
  ##return result:
  ##==============
  
  result <- c(getsResult, plmResult)
  if(!is.null(plmClass)){ class(result) <- plmClass }
  return(result)
  
} #close gets.plm
