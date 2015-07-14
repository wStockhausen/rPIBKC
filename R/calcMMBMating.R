#'
#'@title Calculate projected MMMB at mating.
#'
#'@description Function to calculate MMMB at mating.
#'
#'@param avgSrvData - dataframe with MMB at time of survey
#'@param fshData    - dataframe with fishery data
#'@param type       - type of survey MMB to use ('averaged' or 'raw')
#'@param M    - assumed rate of natural mortality
#'@param t.sf - time from survey to fishery
#'@param t.fm - time from fishery to mating
#'@param hm.pot - handling mortality rate for fixed gear (pot) fisheries
#'@param hm.trl - handling mortality rate for trawl fisheries
#'@param pct.male - assumed male percentage
#'
#'@return list with MMB at mating and other time series
#'
#'@import ggplot2
#'
#'@export
#'
calcMMBMating<-function(avgSrvData,
                        fshData,
                        type='averaged',
                        M=0.18,
                        t.sf=3/12,
                        t.fm=4/12,
                        hm.pot=0.5,
                        hm.trl=0.8,
                        pct.male=0.5){
    #pull out survey MMB data
    idx<-avgSrvData$type==type;
    mmbYrs<-avgSrvData$year[idx];
    mmbSrv<-avgSrvData$value[idx];
    names(mmbSrv)<-mmbYrs;

    #pull out retained catch from fishery data
    idx<-fshData$type=='retained';
    ret<-fshData$catch[idx];
    names(ret)<-fshData$year[idx];

    #pull out discard catch in crab fisheries, convert to mortality
    idx<-(fshData$type=='discard')&(fshData$fishery=='crab fisheries')&(fshData$category=='legal');
    dsc.crb<-hm.pot*fshData$catch[idx];#now mortality
    names(dsc.crb)<-fshData$year[idx];
    idx<-is.na(dsc.crb);
    dsc.crb[idx]<-0;#set NAs to zeros

    #pull out discard catch in groundfish fixed gear fisheries, convert to mortality
    idx<-(fshData$type=='discard')&(fshData$fishery=='groundfish fisheries')&(fshData$gear=='pot');
    dsc.gfp<-pct.male*hm.pot*fshData$catch[idx];#now mortality
    names(dsc.gfp)<-fshData$year[idx];
    idx<-is.na(dsc.gfp);
    dsc.gfp[idx]<-0;#set NAs to zeros

    #pull out discard catch in groundfish trawl gear fisheries, convert to mortality
    idx<-(fshData$type=='discard')&(fshData$fishery=='groundfish fisheries')&(fshData$gear=='trawl');
    dsc.gft<-pct.male*hm.trl*fshData$catch[idx];#now mortality
    names(dsc.gft)<-fshData$year[idx];
    idx<-is.na(dsc.gft);
    dsc.gft[idx]<-0;#set NAs to zeros

    #calculate MMB at mating and total mature male discard mortality
    mmbFsh<-0*mmbSrv;
    mmbMat<-0*mmbSrv;
    retM   <-0*mmbSrv;
    dscTotM<-0*mmbSrv;
    fshTotM<-0*mmbSrv;
    ny    <-length(mmbYrs);
    for (y in names(mmbMat)){
        mmbFsh[y]<-mmbSrv[y]*exp(-M*t.sf);
        retM[y]   <-ret[y];
        dscTotM[y]<-dsc.crb[y]+dsc.gfp[y]+dsc.gft[y];
        fshTotM[y]<-ret[y]+dscTotM[y];
        mmbMat[y] <-(mmbFsh[y]-fshTotM[y])*exp(-M*t.fm);
    }

    dfr<-data.frame(year=mmbYrs,MMB=mmbMat);
    pd<-position_dodge(0.5);
    p <- ggplot(aes_string(x='year',y='MMB'),data=dfr);
    p <- p + geom_point(position=pd,size=3);
    p <- p + geom_line( position=pd,size=1,alpha=1);
    p <- p + ylab("MMB at mating (1000's t)");
    print(p);

    return(list(mmbMat=mmbMat,mmbFsh=mmbFsh,mmbSrv=mmbSrv,retM=retM,dscM=list(tot=dscTotM,gft=dsc.gft,gfp=dsc.gfp,crb=dsc.crb)))
}