#'
#'@title Smooth/interpolate survey data using a random effects(kalman filter) model (REM). 
#'
#'@description Function to smooth/interpolate survey data using a random effects(kalman filter) model (REM).
#'
#'@details This function uses an ADMB random effects model (originally developed by Jim Ianelli 
#'and subsequently modified by William Stockhausen) to smooth/interpolate survey data.
#'
#'smooth survey data 
#'@param srvData - raw survey data dataframe
#'@param type - data type ('abundance' or 'biomass') to average
#'@param sex - sex ('male' or 'female') to average
#'@param category - category ('immature','mature', or 'legal') to average
#'@param pdfType - distribution for CIs
#'@param ci - confidence interval for CIs
#'@param modelPath - path to ADMB REM used for survey averaging
#'@param verbose - flag (T/F) to print intermediate output
#'@param showPlot - flag (T/F) to plot results 
#'
#'@return dataframe with smoothed survey data, with columns
#'\itemize{
#'  \item year = survey year
#'  \item type = 'IV'
#'  \item value = averaged value
#'  \item lci   = lower confidence interval
#'  \item uci   = upper confidence interval
#'}
#'
#'@importFrom PBSmodelling readList
#'
#'@details Smoothing is done using a Kalman filter/Random Effects Model
#'written in ADMB (C++) code. The single estimated parameter is the 
#'ln-scale process error variance for annual changes in survey abundance/biomass
#'modeled as a random walk process. The estimated time series is output.
#'
#'@export
#'
surveyAveraging.REM<-function(srvData,
                              type='biomass',
                              sex='male',
                              category='mature',
                              pdfType='lognormal',
                              ci=0.95,
                              modelPath=getPath2REM(),
                              verbose=FALSE,
                              showPlot=FALSE){
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

    #determine model name and OS
    os<-'OSX';
    model<-basename(modelPath);
    if (substr(model,length(model)-3,length(model))=='.exe') {
        os<-'win';
        model<-substr(model,1,length(model)-4)
    }
    
    #switch to run folder (create if necessary)
    currdir<-getwd();
    on.exit(setwd(currdir));
    path<-file.path(currdir,'admb')
    if (!file.exists(path)) dir.create(path,recursive=TRUE)
    setwd(path);
    if (verbose) cat("Running REM at '",path,"'.\n",sep='');

    #set up input data file to REM
    con<-file(paste(model,'dat',sep='.'),open='wt');
    writeLines(paste(min(sd$year),   "\t#min year"),con);
    writeLines(paste(max(sd$year),   "\t#max year"),con);
    writeLines(paste(length(sd$year),"\t#number of observations"),con);
    writeLines(paste(0,              "\t#uncertainty type (0 = cv's, 1 = sd's)"),con);
    csv<-sd[,c("year","value","cv")];
    writeLines("#year    value     cv",con)
    write.table(csv,file=con,append=TRUE,row.names=FALSE,col.names=FALSE);
    close(con);
    
    #set up commands
    run.cmds<-getRunCommands(os=os,path2model=modelPath,hess=TRUE);
    if (verbose) cat(run.cmds,"\n")
    
    #run the ADMB model
    if (tolower(os)=='win'){
        cat(run.cmds,file="tmp.bat")
        Sys.chmod("tmp.bat",mode='7777')
        system("tmp.bat",wait=TRUE);
    } else {
        cat(run.cmds,file="./tmp.sh")
        Sys.chmod("./tmp.sh",mode='7777')
        system("./tmp.sh",wait=TRUE);
    }
    
    #read model results
    fn.par<-file.path(getwd(),"&&model.par");
    fn.par<-gsub('&&model',tolower(model),fn.par)
    
    res.REM<-readList('rwout.rep')

    #finish off the output
    res<-calcCIs(res.REM$est,res.REM$cv,pdfType=pdfType,ci=ci,verbose=verbose);
    dfr<-rbind(dfr,data.frame(year=res.REM$yrs,type='REM',value=res.REM$est,lci=res$lci,uci=res$uci));

    if (showPlot) plotAvgdData(dfr);
    
    return(dfr);
}