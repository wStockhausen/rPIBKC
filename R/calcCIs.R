#'
#'@title Compute confidence intervals
#'
#'@description Function to compute confidence intervals.
#'
#'@param vals - vector of values
#'@param cvs - vector of cvs
#'@param pdfType - probability distribution for error bars
#'@param ci - confidence interval for error bar plots
#'@param verbose - flag (T/F) to print intermediate output
#'
#'@return list with vectors lci, uci as  named elements
#'
#'@details None.
#'
#'@importFrom stats qlnorm qnorm
#'
#'@export
#'
calcCIs<-function(vals,cvs,pdfType='lognormal',ci=0.95,verbose=FALSE){
    #compute confidence intervals for survey data
    ci<-c((1-ci)/2,1-(1-ci)/2);#confidence intervals
    obs<-vals;
    cv <-cvs;
    if (tolower(pdfType)=='normal'){
        if (verbose) cat('using err type = normal\n')
        sdv<-cv*obs;
        lci<-qnorm(ci[1],mean=obs,sd=sdv);
        uci<-qnorm(ci[2],mean=obs,sd=sdv);
    } else if (tolower(pdfType)=='lognormal'){
        if (verbose) cat('using err type = lognormal\n')
        sdv<-sqrt(log(1+cv^2));
        lci<-qlnorm(ci[1],meanlog=log(obs),sdlog=sdv);
        uci<-qlnorm(ci[2],meanlog=log(obs),sdlog=sdv);
    } else {
        cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
        cat('Error in plotRawData.\n')
        cat("pdfType '",pdfType,"' not recognized!!\n")
        cat("Exiting function.\n")
        cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
        return(NULL)
    }

    return(list(lci=lci,uci=uci));
}