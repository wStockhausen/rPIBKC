#'
#'@title Smooth survey data using a REM (Random Effects[kalman filter] Model) 
#'
#'@description Function to smooth survey data using a REM (Random Effects[kalman filter] Model) 
#'
#'@param srvData - raw survey data dataframe
#'@param type - data type ('abundance' or 'biomass') to average
#'@param sex - sex ('male' or 'female') to average
#'@param category - category ('immature','mature', or 'legal') to average
#'@param pdfType - distribution for CIs
#'@param ci - confidence interval for CIs
#'@param showPlot - flag (T/F) to plot results 
#'
#'@return dataframe with smoothed survey data
#'
#'@export
#'
surveyAveraging.REM<-function(srvData,
                              type='biomass',
                              sex='male',
                              category='mature',
                              pdfType='lognormal',
                              ci=0.95,
                              modelName='',
                              ModelPath=getwd(),
                              showPlot=TRUE){
    #select data
    idx<-(srvData$type==type)&(srvData$sex==sex)&(srvData$category==category);
    sd<-srvData[idx,];

    if(is.null(sd$lci)){
        cis<-calcCIs(sd$value,sd$cv*sd$value,pdfType=pdfType,ci=ci);
        lci<-cis$lci;
        uci<-cis$uci;
    } else {
        lci<-sd$lci;
        uci<-sd$uci;
    }
    dfr<-data.frame(year=sd$year,type='raw',value=sd$value,lci=lci,uci=uci);

    #set up counters
    np<-(n-1)/2;
    ny<-nrow(sd);

    #do averaging
    val <-vector(mode='numeric',length=ny)+NA;
    cv  <-vector(mode='numeric',length=ny)+NA;
    for (y in (1+np):ny){
        val[y] <-0;
        twgt   <-0;
        if (y<=(ny-np)) {
            ns<-(-np):np;
        } else {
            ns<-(-np):(np-(y-(ny-np)));
            cat('ny = ',ny,', y = ',y,', ns = ',ns,'\n',sep='')
        }
        for (n in ns){
            var   <-(sd$cv[y+n]*sd$value[y+n])^2;
            wgt   <-1/var;
            twgt  <-twgt+wgt;
            val[y]<-val[y]+wgt*sd$value[y+n];
        }#n
        val[y]<-val[y]/twgt;
        var   <-1/twgt;
        cv[y] <-sqrt(var)/val[y];
    }#y

    res<-calcCIs(val,cv,pdfType=pdfType,ci=ci);
    dfr<-rbind(dfr,data.frame(year=sd$year,type='averaged',value=val,lci=res$lci,uci=res$uci));

    if (showPlot) plotAvgdData(dfr);
    
    return(dfr);
}