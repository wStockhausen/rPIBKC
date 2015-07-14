#'
#'@title Calculate projected MMMB at mating.
#'
#'@description Function to calculate projected MMMB at mating.
#'
#'@param mmbSrvCurr - current MMB at time of survey
#'@param Fm         - assumed fishing mortality rate
#'@param theta      - ratio of discard mortality to MMB at fishing
#'@param M    - assumed rate of natural mortality
#'@param t.sf - time from survey to fishery
#'@param t.fm - time from fishery to mating
#'@param pct.male - assumed male percentage
#'
#'@return projected MMB at mating
#'
#'@export
#'
calcPrjMMB<-function(mmbSrvCurr,
                     Fm,
                     theta,
                     M=0.18,
                     t.sf=3/12,
                     t.fm=4/12){

    #projected MMB at time of fishery
    mmbFshPrj<-mmbSrvCurr*exp(-M*t.sf);

    #projected retained catch
    prjRet<-(1-exp(-Fm))*mmbFshPrj;

    #projected discard mortality on MMB
    prjDsc<-theta*mmbFshPrj;

    #projected MMB at mating
    mmbMatPrj<-(mmbFshPrj-prjRet-prjDsc)*exp(-M*t.fm);

    return(list(mmb=mmbMatPrj,retM=prjRet,dscM=prjDsc));
}
