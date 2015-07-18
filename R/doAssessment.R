#'
#'@title Do assessment.
#'
#'@description Function to do the assessment
#'
#'@export
#'
doAssessment<-function(assYr=2014,
                       srvData=NULL,
                       fshData=NULL,
                       srvTypeForMMB='IV',
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
                       ci=0.95){
    #get data (if necssary) and plot it
    lstData<-plotRawData(srvData,fshData,pdfType,ci);
    if (is.null(srvData)){
        srvData<-lstData$srvData;
        fshData<-lstData$fshData;
    }

    #smooth survey data
    if (srvTypeForMMB=='IV'){
        avgSrvData<-surveyAveraging.InvVar(srvData,
                                           type='biomass',
                                           sex='male',
                                           category='mature',
                                           n=nYrsSrvAvg,
                                           pdfType=pdfType,
                                           ci=ci);
    } else if (srvTypeForMMB=='REM'){
        avgSrvData<-surveyAveraging.REM(srvData,
                                        type='biomass',
                                        sex='male',
                                        category='mature',
                                        n=nYrsSrvAvg,
                                        pdfType=pdfType,
                                        ci=ci);
    } else {
        cat("Survey average method",srvTypeForMMB,"not recognized!\n")
        cat('Aborting...\n')
        return(NULL);
    }

    #calculate MMB at mating time series
    lstMMB<-calcMMBMating(avgSrvData,
                          fshData,
                          type=srvTypeForMMB,
                          M=M,
                          t.sf=t.sf,
                          t.fm=t.fm,
                          hm.pot=hm.pot,
                          hm.trl=hm.trl,
                          pct.male=pct.male);
    mmbSrvCurr<-lstMMB$mmbSrv[as.character(assYr)];
    cat("Current MMB-at-survey:",mmbSrvCurr,'\n')

    #calculate Bmsy by averaging MMB at mating over specified time intervals
    Bmsy<-calcBmsy(lstMMB$mmbMat,years=yrsForBmsy);
    cat("Bmsy:",Bmsy,'\n')

    #calculate 'theta', the average discard mortality exploitation rate
    theta<-calcTheta(lstMMB,assYr=assYr,n=nYrsTheta)[2];
    cat("theta:",theta,'\n')

    #calculate OFL quantities
    lstOFL<-calcOFL(mmbSrvCurr,
                    Bmsy,
                    theta,
                    M=M,
                    gamma=gamma,
                    alpha=alpha,
                    beta=beta,
                    t.sf=t.sf,
                    t.fm=t.fm);
    cat('retOFL:',lstOFL$retOFL,"dscOFL:",lstOFL$dscOFL,"totOFL:",lstOFL$retOFL+lstOFL$dscOFL,'\n')
    cat('Fofl:',lstOFL$Fofl,"prjMMB:",lstOFL$prjMMB,'\n')

}