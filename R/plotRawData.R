#'
#'@title Plot raw survey and fishery data.
#'
#'@description Function to plot raw survey and fishery data.
#'
#'@param srvData - survey data dataframe, path to csv file, or NULL
#'@param fshData - fisheries data dataframe, path to csv file, or NULL
#'@param yr2 - starting year for 2nd arithmetic scale plot
#'@param pdfType - probability distribution for error bars
#'@param ci - confidence interval for error bar plots
#'@param showPlot - flag (T/F) to show plots immediately
#'
#'@return list with named elements
#'\itemize{
#'  \item srvData - dataframe w/ abundance is in milllions and biomass is in 1000's t
#'  \item fshData - dataframe w/ catch biomass is in t
#'  \item plots - list with ggplot2 objects 
#'  \itemize{
#'      \item pS1
#'      \item pS2
#'      \item pS3
#'      \item pF1
#'      \item pF2
#'  \}
#'\}
#'
#'@import ggplot2
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
plotRawData<-function(srvData=NULL,
                      fshData=NULL,
                      yr2=1990,
                      pdfType='lognormal',
                      ci=0.95,
                      showPlot=FALSE
                      ){
    if (is.null(srvData)|is.character(srvData)) srvData<-getSurveyData(srvData);
    if (is.null(fshData)|is.character(fshData)) fshData<-getFisheryData(fshData);

    #---survey data---
    #compute confidence intervals for survey data
    res<-calcCIs(srvData$value,srvData$cv,pdfType=pdfType,ci=ci);
    srvData$lci<-res$lci;
    srvData$uci<-res$uci;

    #plot survey biomass
    bSD1<-srvData[srvData$type=='biomass',];
    ymx <- sort(bSD1$uci,decreasing=TRUE)[6];
    pd<-position_dodge(0.5);
    p <- ggplot(aes_string(x='year',y='value',colour='sex',fill='sex',shape='sex'),data=bSD1);
    p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd,width=0.8,linetype=1);
    p <- p + geom_point(position=pd,size=3);
    p <- p + geom_line( position=pd,size=1,alpha=1);
    p <- p + coord_cartesian(ylim=c(0,ymx));
    p <- p + ylab("Survey Biomass (1000's t)");
    p <- p + facet_grid(category~.);
    pS1 <- p;
    if (showPlot) print(pS1);

    bSD2<-bSD1[bSD1$year>=yr2,];
    ymx <- sort(bSD2$uci,decreasing=TRUE)[1];
    pS2 <- p %+% bSD2
    pS2 <- pS2 + coord_cartesian(ylim=c(0,ymx));
    if (showPlot) print(pS2);

    bSD3<-bSD1;
    bSD3$value<-log10(bSD3$value);
    bSD3$uci<-log10(bSD3$uci);
    bSD3$lci<-log10(bSD3$lci);
    ymx <- sort(bSD3$uci,decreasing=TRUE)[1];
    pS3 <- p %+% bSD3;
    pS3 <- pS3 + coord_cartesian(ylim=NULL);
    pS3 <- pS3 + ylab("log10-scale Survey Biomass (1000's t)");
    if (showPlot) print(pS3);

    #--fishery data--
    #plot fishery data
    pd<-position_dodge(0.5);
    p <- ggplot(aes_string(x='year',y='catch',colour='category',fill='gear',shape='gear',linetype='fishery'),data=fshData);
    p <- p + geom_point(position=pd,size=3);
    p <- p + geom_line( position=pd,size=1,alpha=1);
    p <- p + ylab("Fishery Catch (1000's t)");
    p <- p + facet_grid(type~.,scales="free_y");
    pF1 <- p;
    if (showPlot) print(pF1);

    pF2 <- p %+% fshData[fshData$year>=1990,];
    if (showPlot) print(pF2);

    return(invisible(list(srvData=srvData,fshData=fshData,
                          plots=list(pS1=pS1,pS2=pS2,pS3=pS3,pF1=pF1,pF2=pF2))));
}

