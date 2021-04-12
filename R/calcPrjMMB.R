#'
#'@title Calculate projected MMMB at mating based on a fishing mortality rate.
#'
#'@description Function to calculate projected MMMB at mating based on a fishing mortality rate.
#'
#'@details MMB-at-mating is projected from survey MMB for a given directed fishing mortality using:
#'\enumerate{
#'  \item \eqn{MMB_f = MMB_s \cdot exp(-M \cdot t_{sf})}
#'  \item \eqn{M_r = MMB_f \cdot (1-exp(-F))}
#'  \item \eqn{M_d = \theta \cdot MMB_f}
#'  \item \eqn{MMB_m = [MMB_f - (M_r + M_d)] \cdot exp(-M \cdot t_{fm})}
#'}
#'where \eqn{MMB_s} is MMB at the time of the survey, \emph{M} is the rate of natural mortality, \eqn{t_{sf}}
#'is the time (as a fraction of the year) between the survey and fishery, \eqn{MMB_f} is MMB just before 
#'the fishery, \emph{F} is the retained mortality rate in the directed fishery, \eqn{M_r} is retained catch mortality,
#'\eqn{\theta} is the fraction of \eqn{MMB_f} killed as discards, \eqn{M_d} is discard catch mortality,
#'and \eqn{t_{fm}} is the time (as fraction of the year) between the fishery and mating.
#'
#'@param mmbSrvCurr - current MMB at time of survey
#'@param Fm         - assumed fishing mortality rate
#'@param theta      - \eqn{\theta}, the average ratio of discard mortality to MMB at fishing
#'@param M    - assumed rate of natural mortality
#'@param t.sf - time from survey to fishery
#'@param t.fm - time from fishery to mating
#'@param verbose - flag (T/F) to print intermediate output
#'
#'@return list with elements:
#'\itemize{
#'  \item mmb  = mmbMatPrj (t)
#'  \item retM = retained mortality (t)
#'  \item dscM = discard mortality on MMB (t)
#'}
#'
#'@export
#'
calcPrjMMB<-function(mmbSrvCurr,
                     Fm,
                     theta,
                     M=0.18,
                     t.sf=3/12,
                     t.fm=4/12,
                     verbose=FALSE){

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
