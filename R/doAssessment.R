#'
#'@title Do assessment.
#'
#'@description Function to do the assessment
#'
#'@param assYr - year of assessment
#'@param srvData - survey data dataframe, path to csv file, or NULL
#'@param fshData - fisheries data dataframe, path to csv file, or NULL
#'@param avgTypeForMMB - flag indicating averaging type for survey data ('raw', 'IV' or 'REM')
#'@param yrsForBmsy - years for Bmsy calculation
#'@param nYrsSrvAvg - 
#'@param nYrsTheta - 
#'@param M - natural mortality
#'@param t.sf - time from survey to fishery
#'@param t.fm - time from fishery to mating
#'@param hm.pot - pot fisheries handling mortality rate
#'@param hm.trl - trawl fisheries handling mortality rate
#'@param pct.male - assumed male percentage
#'@param gamma - multiplicative factor on M for maxFofl
#'@param alpha - x-intercept of sloping control line
#'@param beta - threshold mmb/Bmsy to allow a directed fishery
#'@param pdfType - probability distribution for error bars
#'@param ci - confidence interval for error bar plots
#'@param verbose - flag (T/F) to print intermediate output
#'@param showPlot - flag (T/F) to plot results 
#'
#'@return list consisting of elements
#'\itemize{
#'  \item data - list with elements:
#'  \itemize{
#'    \item fshData - dataframe with fishery data
#'    \item srvData - dataframe with 'raw' survey data
#'    \item avgSrvData - dataframe with 'smoothed' survey data
#'    \item plots.RawData - list of ggplot2 objects
#'    \item plots.AvgdData - list of ggplot2 objects
#'  }
#'  \item lstMMB - list from calcMMBmating:
#'  \itemize{
#'    \item mmbMat = MB at maturity
#'    \item mmbFsh = MMB at fishery time
#'    \item mmbSrv = MMB at survey time
#'    \item retM = retained mortality
#'    \item dscM = list with elements: 
#'    \itemize{
#'      \item tot = total discard mortality
#'      \item gft = groundfish trawl fisheries mortality
#'      \item gfp = groundfish pot fisheries mortality
#'      \item crb = crab fisheries discard mortality
#'    }
#'  \item plots = list with elements: 
#'    \itemize{
#'      \item MMB = MMB time series ggplot2 plot object 
#'    }
#'  }
#'  \item inputs.OFL - list with elements 
#'  \itemize{
#'    \item mmbSrvCurr
#'    \item Bmsy
#'    \item theta
#'    \item M
#'    \item gamma
#'    \item alpha
#'    \item beta
#'    \item t.sf
#'    \item t.fm
#'  }
#'  \item lstOFL - list from calcOFL
#'  \itemize{
#'    \item maxFofl = max allowed Fofl (=gamma*M)
#'    \item Bmsy   = Bmsy
#'    \item Fofl   = Fofl
#'    \item prjMMB = projected MMB
#'    \item retOFL = retained portion of total OFL
#'    \item dscOFL = discard portion of total OFL
#'  }
#'}
#'
#'@export
#'
doAssessment<-function(assYr=2014,
                       recentYr=1990,
                       srvData=NULL,
                       fshData=NULL,
                       avgTypeForMMB='IV',
                       yrsForBmsy=c(1980:1984,1990:1997),
                       nYrsSrvAvg=3,
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
    if (is.null(srvData)|is.character(srvData)){srvData<-getSurveyData(srvData);}
    if (is.null(fshData)|is.character(fshData)){fshData<-getFisheryData(fshData);}
    
    #plot raw data
    plts.RawData<-plotRawData(srvData,fshData,pdfType,ci,verbose=verbose,showPlot=showPlot);

    #smooth survey data
    if (avgTypeForMMB=='IV'){
        avgSrvData<-surveyAveraging.InvVar(srvData,
                                           type='biomass',
                                           sex='male',
                                           category='mature',
                                           n=nYrsSrvAvg,
                                           pdfType=pdfType,
                                           ci=ci,
                                           verbose=verbose,
                                           showPlot=FALSE);
    } else if (avgTypeForMMB=='REM'){
        avgSrvData<-surveyAveraging.REM(srvData,
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
    plts.AvgdData<-plotAvgdData(dfr,yr2=recentYear,showPlot=showPlot);

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
                        t.fm=t.fm);
    lstOFL<-calcOFL(mmbSrvCurr,
                    Bmsy,
                    theta,
                    M=M,
                    gamma=gamma,
                    alpha=alpha,
                    beta=beta,
                    t.sf=t.sf,
                    t.fm=t.fm,
                    verbose=verbose);
    if (verbose) cat('retOFL:',lstOFL$retOFL,"dscOFL:",lstOFL$dscOFL,"totOFL:",lstOFL$retOFL+lstOFL$dscOFL,'\n')
    if (verbose) cat('Fofl:',lstOFL$Fofl,"prjMMB:",lstOFL$prjMMB,'\n')

    return(list(data=list(fshData=fshData,srvData=srvData,avgSrvData=avgSrvData),
                plots.RawData=plts.RawData,plots.AvgdData=plts.AvgdData,
                lstMMB=lstMMB,inputs.OFL=lstOFL.inputs,lstOFL=lstOFL));
}