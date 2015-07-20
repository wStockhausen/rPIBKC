#'
#'@title Plot averaged data.
#'
#'@description Function to plot averaged (and raw) data using output from one of the surveyAveraging functions.
#'
#'@param dfr - dataframe output from one of the surveyAveraging functions
#'@param yr2 - starting year for 2nd arithmetic scale plot
#'@param showPlot - flag (T/F) to show plots immediately
#'
#'@return list with ggplot2 objects as elements arScl1, arScl2, lnScl
#'
#'@import ggplot2
#'
#'@export
#'
plotAvgdData<-function(dfr,yr2=1990,showPlot=FALSE){
    
    types<-tolower(unique(dfr$type));
    ylab<-"Survey Biomass (1000's t)";
    if ('abundance' %in% types) ylab <- "Survey Abundance (millions)";
    bSD1<-dfr;
    ymx <- sort(bSD1$uci,decreasing=TRUE)[6];
    pd<-position_dodge(0.5);
    p <- ggplot(aes_string(x='year',y='value',colour='type',fill='type',shape='type'),data=bSD1);
    p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd,width=0.8,linetype=1);
    p <- p + geom_point(position=pd,size=3,alpha=0.8);
    p <- p + geom_line( position=pd,size=1,alpha=1,alpha=0.7);
    if (!is.na(ymx)) p <- p + coord_cartesian(ylim=c(0,ymx));
    p <- p + ylab(ylab);
    p1 <- p;
    if (showPlot) print(p1);

    bSD2<-bSD1[bSD1$year>=yr2,];
    ymx <- sort(bSD2$uci,decreasing=TRUE)[2];
    p2 <- p %+% bSD2
    if (!is.na(ymx)) p2 <- p2 + coord_cartesian(ylim=c(0,ymx));
    if (showPlot) print(p2);

    bSD3<-bSD1;
    bSD3$value<-log(bSD3$value);
    bSD3$lci<-log(bSD3$lci);
    bSD3$uci<-log(bSD3$uci);
    p3 <- p %+% bSD3
    p3 <- p3 + ylab(paste("ln-scale",ylab));
    p3 <- p3 + coord_cartesian(ylim=NULL);
    if (showPlot) print(p3);
    return(invisible(list(arScl1=p1,arScl2=p2,lnScl=p3)))
}