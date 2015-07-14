#'
#'@title Plot raw survey and fishery data.
#'
#'@description Function to plot raw survey and fishery data.
#'
#'@param srvData - survey data dataframe, path to csv file, or NULL
#'@param fshData - fisheries data dataframe, path to csv file, or NULL
#'@param pdfType - probability distribution for error bars
#'@param ci - confidence interval for error bar plots
#'
#'@return list with srvData, fshData as elements. For survey data, abundance is in
#'milllions and biomass is in 1000's t. For fishery data, catch biomass is in t.
#'
#'@import ggplot2
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
plotRawData<-function(srvData=NULL,
                      fshData=NULL,
                      pdfType='lognormal',
                      ci=0.95
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
    p1 <- p;
    print(p1)

    bSD2<-bSD1[bSD1$year>=1990,];
    ymx <- sort(bSD2$uci,decreasing=TRUE)[1];
    p2 <- p %+% bSD2
    p2 <- p2 + coord_cartesian(ylim=c(0,ymx));
    print(p2);

    bSD3<-bSD1;
    bSD3$value<-log10(bSD3$value);
    bSD3$uci<-log10(bSD3$uci);
    bSD3$lci<-log10(bSD3$lci);
    ymx <- sort(bSD3$uci,decreasing=TRUE)[1];
    p3 <- p %+% bSD3;
    p3 <- p3 + coord_cartesian(ylim=NULL);
    p3 <- p3 + ylab("log10-scale Survey Biomass (1000's t)");
    print(p3);

    #--fishery data--
    #plot fishery data
    pd<-position_dodge(0.5);
    p <- ggplot(aes_string(x='year',y='catch',colour='category',fill='gear',shape='gear',linetype='fishery'),data=fshData);
    p <- p + geom_point(position=pd,size=3);
    p <- p + geom_line( position=pd,size=1,alpha=1);
    p <- p + ylab("Fishery Catch (1000's t)");
    p <- p + facet_grid(type~.,scales="free_y");
    pF1 <- p;
    print(pF1);

    pF2 <- p %+% fshData[fshData$year>=1990,];
    print(pF2);

    return(invisible(list(srvData=srvData,fshData=fshData)));
}

