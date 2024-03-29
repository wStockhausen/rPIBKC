#'
#'@title Smooth/interpolate survey data using a random effects/kalman filter model (RE). 
#'
#'@description Function to smooth/interpolate survey data using a random effects/kalman filter model (RE).
#'
#'@details This function uses an ADMB random effects model (originally developed by Jim Ianelli 
#'and subsequently modified by William Stockhausen) to smooth/interpolate survey data.
#'
#' 
#'@param maxYr - max year for output (typically assessment year)
#'@param srvData - raw survey data dataframe
#'@param type - data type ('abundance' or 'biomass') to average
#'@param sex - sex ('male' or 'female') to average
#'@param category - category ('immature','mature', or 'legal') to average
#'@param pdfType - distribution for CIs
#'@param ci - confidence interval for CIs
#'@param modelPath - path to ADMB RE model used for survey averaging
#'@param verbose - flag (T/F) to print intermediate output
#'@param showPlot - flag (T/F) to plot results 
#'
#'@return list with a dataframe ('dfr') and another list ('lst') as elements. T
#'he dataframe is the smoothed survey data, with columns
#'\itemize{
#'  \item year = survey year
#'  \item type = 'RE'
#'  \item value = averaged or predicted value
#'  \item lci   = lower confidence interval
#'  \item uci   = upper confidence interval
#'}
#'The list contains other results from the model optimization, including
#'\itemize{
#'  \item objFun = the final objective function value
#'  \item maxGrad = the max gradient 
#'  \item sdrepSdLam = the estimated process error standard deviation, on the arithmetic scale
#'  \item sdrepSdLam.sd = the standard deviation of the estimated process error standard deviation, on the arithmetic scale
#'  \item and others
#'}
#'
#'@importFrom PBSmodelling readList
#'@importFrom utils write.table
#'
#'@details Smoothing is done using a Kalman Filter/Random Effects model
#'written in ADMB (C++) code. The single estimated parameter is the 
#'ln-scale process error variance for annual changes in survey abundance/biomass
#'modeled as a random walk process. The estimated time series is output.
#'
#'@export
#'
surveyAveraging.RE<-function(maxYr,
                             srvData,
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
        cis<-calcCIs(sd$value,sd$cv,pdfType=pdfType,ci=ci);
        lci<-cis$lci;
        uci<-cis$uci;
    } else {
        lci<-sd$lci;
        uci<-sd$uci;
    }
    dfr<-data.frame(year=sd$year,type='raw',value=sd$value,lci=lci,uci=uci);

    #determine model name and OS
    os<-'osx';
    model<-basename(modelPath);
    if (verbose) message(paste0("substr: '",substr(model,nchar(model)-3,nchar(model)),"'"));
    if (substr(model,nchar(model)-3,nchar(model))=='.exe') {
        os<-'win';
        model<-substr(model,1,nchar(model)-4);
    }
    if (verbose) message(paste0("assumed OS is '",os,"'"));
    
    #switch to run folder (create if necessary)
    currdir<-getwd();
    on.exit(setwd(currdir));
    path<-file.path(currdir,'admb')
    if (!file.exists(path)) dir.create(path,recursive=TRUE)
    setwd(path);
    if (verbose) cat("Running RE at '",path,"'.\n",sep='');

    #set up input data file to RE
    con<-file(paste(model,'dat',sep='.'),open='wt');
    writeLines(paste(min(sd$year),   "\t#min year"),con);
    writeLines(paste(maxYr,          "\t#max year"),con);#--can be > max(survey year) for predictions
    writeLines(paste(length(sd$year),"\t#number of observations"),con);
    writeLines(paste(0,              "\t#uncertainty type (0 = cv's, 1 = sd's)"),con);
    csv<-sd[,c("year","value","cv")];
    writeLines("#year    value     cv",con)
    write.table(csv,file=con,append=TRUE,row.names=FALSE,col.names=FALSE);
    close(con);
    
    #set up commands
    fsep<-.Platform$file.sep; if (os=="win") fsep<-"\\";
    run.cmds<-getRunCommands(os=os,path2model=file.path(dirname(modelPath),basename(modelPath),fsep=fsep),hess=TRUE);
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
    # fn.par<-file.path(getwd(),"&&model.par");
    # fn.par<-gsub('&&model',tolower(model),fn.par)
    res.RE<-PBSmodelling::readList('rwout.rep')

    #finish off the output
    res<-calcCIs(res.RE$est,res.RE$cv,pdfType=pdfType,ci=ci,verbose=verbose);
    dfr<-rbind(dfr,data.frame(year=res.RE$yrs,type='RE',value=res.RE$est,lci=res$lci,uci=res$uci));

    if (showPlot) plotAvgdData(dfr);
    
    return(list(dfr=dfr,lst=res.RE));
}