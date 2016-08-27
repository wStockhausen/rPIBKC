#'
#'@title Do assessment
#'
#'@description Function to do the assessment.
#'
#'@param assYr - year of assessment
#'@param srvData - survey data dataframe, path to csv file, or NULL
#'@param fshData - fisheries data dataframe, path to csv file, or NULL
#'@param recentYear - recent year to set time axis for some plots
#'@param avgTypeForMMB - flag indicating averaging type for survey data ('raw', 'IV' or 'RE')
#'@param yrsForBmsy - years for \eqn{B_{MSY}} calculation
#'@param nYrsSrvIV - number of years to include in inverse variance averages
#'@param nYrsTheta - number of years to average for \eqn{\theta} calculation
#'@param M - rate of natural mortality
#'@param t.sf - time from survey to fishery
#'@param t.fm - time from fishery to mating
#'@param hm.pot - pot fisheries handling mortality rate
#'@param hm.trl - trawl fisheries handling mortality rate
#'@param pct.male - assumed male percentage
#'@param gamma - value for the Tier 4 \eqn{\gamma} constant (in Tier 4: \eqn{F_{OFL_{max}} = \gamma \cdot M})
#'@param alpha - value for the Tier 4 \eqn{\alpha} constant (the x-intercept of the sloping control rule)
#'@param beta - value for the Tier 4 \eqn{\beta} constant (the threshold for \eqn{MMB/B_{MSY}} to allow directed fishing)
#'@param pdfType - probability distribution for error bars
#'@param ci - confidence interval for error bar plots
#'@param verbose - flag (T/F) to print intermediate output
#'@param showPlot - flag (T/F) to plot results 
#'
#'@details Output units are in t for biomass, ones for abundance.
#'
#'@return list consisting of the following elements: 
#'\itemize{
#'  \item data - list with elements:
#'  \itemize{
#'    \item fshData - dataframe with fishery data
#'    \item srvData - dataframe with 'raw' survey data
#'    \item avgSrvData - dataframe with 'smoothed' survey data
#'    \item plots.RawData - list of ggplot2 objects
#'    \item plots.AvgdData - list of ggplot2 objects
#'  }
#'  \item lstMMB - list from \code{\link{calcMMBmating}}:
#'  \itemize{
#'    \item mmbMat = MB at maturity (t)
#'    \item mmbFsh = MMB at fishery time (t)
#'    \item mmbSrv = MMB at survey time (t)
#'    \item retM = retained mortality (t)
#'    \item dscM = list with elements: 
#'    \itemize{
#'      \item tot = total discard mortality (t)
#'      \item gft = groundfish trawl fisheries mortality (t)
#'      \item gfp = groundfish pot fisheries mortality (t)
#'      \item crb = crab fisheries discard mortality (t)
#'    }
#'  \item plots = list with elements: 
#'    \itemize{
#'      \item MMB = MMB time series ggplot2 plot object 
#'    }
#'  }
#'  \item inputs.OFL - list of inputs to \code{\link{calcOFL}}, with elements: 
#'  \itemize{
#'    \item mmbSrvCurr - "current" value of MMB at survey time
#'    \item Bmsy - \eqn{B_{MSY}} from \code{\link{calcBmsy}}
#'    \item theta - \eqn{\theta}, from \code{\link{calcTheta}}
#'    \item M - rate of natural mortality, \eqn{M}
#'    \item gamma - value for the Tier 4 \eqn{\gamma} constant (in Tier 4: \eqn{F_{OFL_{max}} = \gamma \cdot M})
#'    \item alpha - value for the Tier 4 \eqn{\alpha} constant
#'    \item beta - value for the Tier 4 \eqn{\beta} constant
#'    \item t.sf - time from survey to fishery (as fraction of year)
#'    \item t.fm - time from fishery to mating (as fraction of year)
#'  }
#'  \item lstOFL - list result from \code{\link{calcOFL}}, with elements:
#'  \itemize{
#'    \item maxFofl = max allowed \eqn{F_{OFL_{max}} (= \gamma \cdot M)})
#'    \item Bmsy   = \eqn{B_{MSY}} (in t)
#'    \item Fofl   = \eqn{F_{OFL}} (in t)
#'    \item prjMMB = projected MMB (in t)
#'    \item retOFL = retained portion of total OFL (in t)
#'    \item dscOFL = discard portion of total OFL (in t)
#'  }
#'}
#'
#'@export
#'
doAssessment<-function(assYr=2014,
                       recentYear=1990,
                       srvData=NULL,
                       fshData=NULL,
                       avgTypeForMMB='IV',
                       yrsForBmsy=c(1980:1984,1990:1997),
                       nYrsSrvIV=3,
                       nYrsTheta=3,
                       M=0.18,
                       t.sf=3/12,
                       t.fm=4/12,
                       hm.pot=0.5,
                       hm.trl=0.8,
                       pct.male=0.5,
                       gamma=1.0,
                       alpha=0.1,
                       beta=0.25,
                       pdfType='lognormal',
                       ci=0.95,
                       verbose=FALSE,
                       showPlot=FALSE){
    
    #get data (if necessary) 
    if (is.null(srvData)|is.character(srvData)){
        srvData<-getSurveyData(srvData);
        View(srvData);
    }
    if (is.null(fshData)|is.character(fshData)){
        fshData<-getFisheryData(fshData);
        View(fshData);
    }
    
    #plot raw data
    plts.RawData<-plotRawData(srvData,fshData,
                              yr2=recentYear,pdfType=pdfType,ci=ci,
                              verbose=verbose,showPlot=showPlot);

    #smooth survey data
    if (avgTypeForMMB=='IV'){
        avgSrvData<-surveyAveraging.IV(srvData,
                                       type='biomass',
                                       sex='male',
                                       category='mature',
                                       n=nYrsSrvIV,
                                       pdfType=pdfType,
                                       ci=ci,
                                       verbose=verbose,
                                       showPlot=FALSE);
    } else if (avgTypeForMMB=='RE'){
        avgSrvData<-surveyAveraging.RE(srvData,
                                        type='biomass',
                                        sex='male',
                                        category='mature',
                                        pdfType=pdfType,
                                        ci=ci,
                                        verbose=verbose,
                                        showPlot=FALSE);
    } else {
        cat("Survey average method",avgTypeForMMB,"not recognized!\n")
        cat('Aborting...\n')
        return(NULL);
    }
    
    #plot survey averaging results
    plts.AvgdData<-plotAvgdData(avgSrvData,yr2=recentYear,verbose=verbose,showPlot=showPlot);

    #calculate MMB at mating time series
    lstMMB<-calcMMBMating(fshData,
                          avgSrvData,
                          avgType=avgTypeForMMB,
                          M=M,
                          t.sf=t.sf,
                          t.fm=t.fm,
                          hm.pot=hm.pot,
                          hm.trl=hm.trl,
                          pct.male=pct.male);
    mmbSrvCurr<-lstMMB$mmbSrv[as.character(assYr)];
    if (verbose) cat("Current MMB-at-survey:",mmbSrvCurr,'\n')

    #calculate Bmsy by averaging MMB at mating over specified time intervals
    Bmsy<-calcBmsy(lstMMB$mmbMat,years=yrsForBmsy);
    if (verbose) cat("Bmsy:",Bmsy,'\n')

    #calculate 'theta', the average discard mortality exploitation rate
    theta<-calcTheta(lstMMB,assYr=assYr,n=nYrsTheta)[2];
    if (verbose) cat("theta:",theta,'\n')

    #calculate OFL quantities
    lstOFL.inputs<-list(mmbSrvCurr=mmbSrvCurr,
                        Bmsy=Bmsy,
                        theta=theta,
                        M=M,
                        gamma=gamma,
                        alpha=alpha,
                        beta=beta,
                        t.sf=t.sf,
                        t.fm=t.fm,
                        pct.male=pct.male);
    lstOFL<-calcOFL(mmbSrvCurr,
                    Bmsy,
                    theta,
                    M=M,
                    gamma=gamma,
                    alpha=alpha,
                    beta=beta,
                    t.sf=t.sf,
                    t.fm=t.fm,
                    pct.male=pct.male,
                    verbose=verbose);
    if (verbose) cat('retOFL:',lstOFL$retOFL,"dscOFL:",lstOFL$dscOFL,"totOFL:",lstOFL$retOFL+lstOFL$dscOFL,'\n')
    if (verbose) cat('Fofl:',lstOFL$Fofl,"prjMMB:",lstOFL$prjMMB,'\n')

    return(list(data=list(fshData=fshData,srvData=srvData,avgSrvData=avgSrvData),
                plots.RawData=plts.RawData,plots.AvgdData=plts.AvgdData,
                lstMMB=lstMMB,inputs.OFL=lstOFL.inputs,lstOFL=lstOFL));
}