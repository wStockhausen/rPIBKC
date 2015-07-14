#'
#'@title Smooth survey data using inverse-variance averaging
#'
#'@description Function to smooth survey data using inverse-variance averaging
#'
#'@param srvData - survey data dataframe
#'
#'@return dataframe with smoothed survey data
#'
#'@export
#'
surveyAveraging.InvVar<-function(srvData,
                                 type='biomass',
                                 sex='male',
                                 category='mature',
                                 n=3,
                                 pdfType='lognormal',
                                 ci=0.95){
    #select data
    idx<-(srvData$type==type)&(srvData$sex==sex)&(srvData$category==category);
    sd<-srvData[idx,];

    if(is.null(sd$lci)){
        lci<-NA;
        uci<-NA;
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

    bSD1<-dfr;
    ymx <- sort(bSD1$uci,decreasing=TRUE)[6];
    pd<-position_dodge(0.5);
    p <- ggplot(aes_string(x='year',y='value',colour='type',fill='type',shape='type'),data=bSD1);
    p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd,width=0.8,linetype=1);
    p <- p + geom_point(position=pd,size=3);
    p <- p + geom_line( position=pd,size=1,alpha=1);
    if (!is.na(ymx)) p <- p + coord_cartesian(ylim=c(0,ymx));
    p <- p + ylab("Survey Biomass (1000's t)");
    p1 <- p;
    print(p1)

    bSD2<-bSD1[bSD1$year>=1990,];
    ymx <- sort(bSD2$uci,decreasing=TRUE)[1];
    p2 <- p %+% bSD2
    if (!is.na(ymx)) p2 <- p2 + coord_cartesian(ylim=c(0,ymx));
    print(p2);

    return(dfr);
}