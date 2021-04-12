#'
#'@title Calculate the OFL using a Tier 4 approach.
#'
#'@description Function to calculate the OFL using a Tier 4 approach.
#'
#'@details In Tier 4, the \eqn{F_{OFL}} is derived from a "kinked" harvest control rule
#'based on the ratio of MMB-at-mating for the assessment year to \eqn{B_{MSY}}. However,
#'when the assessment year is the current year (so that the OFL is calculated for the
#'upcoming fishing season), the MMB-at-mating itself depends what \emph{will be} caught by the fishery,
#'which in turns depends on the OFL and (possibly) the \eqn{F_{OFL}}. Consequently, the calculation for
#'OFL uses the following iterative procedure:
#'\enumerate{
#'  \item "guess" a value for \eqn{F_{OFL}} (\eqn{F_{OFL_{max}} = \gamma \cdot M} is used) 
#'  \item determine the OFL corresponding to fishing at \eqn{F_{OFL}}
#'  \item project MMB-at-mating from the "current" survey MMB (raw or averaged) and the OFL
#'  \item use the harvest control rule to determine the \eqn{F_{OFL}} corresponding to the projected MMB-at-mating
#'  \item update the "guess" in 1. for the result in 4.
#'  \item repeat steps 2-5 until the process has converged, yielding self-consistent values for \eqn{F_{OFL}} and MMB-at-mating
#'}
#'
#'@param mmbSrvCurr - "current" MMB at time of survey
#'@param Bmsy - \eqn{B_{MSY}} (see \code{\link{calcBmsy}})
#'@param theta - value for \eqn{\theta} (see  \code{\link{calcTheta}})
#'@param M - natural mortality rate
#'@param gamma - value for the Tier 4 \eqn{\gamma} constant (in Tier 4: \eqn{F_{OFL_{max}} = \gamma \cdot M})
#'@param alpha - value for the Tier 4 \eqn{\alpha} constant (the x-intercept of the sloping control rule)
#'@param beta - value for the Tier 4 \eqn{\beta} constant (the threshold for \eqn{MMB/B_{MSY}} to allow directed fishing)
#'@param t.sf - time (fraction of year) from survey to (pulse) fishery
#'@param t.fm - time (as fraction of year) from (pulse) fishery to mating
#'@param pct.male - assumed male percentage
#'@param verbose - flag (T/F) to print intermediate output
#'
#'@return List with elements:
#'\itemize{
#'  \item prjMMB = projected MMB to time of mating (in t)
#'  \item Bmsy   = Tier 4 \eqn{B_{MSY}} (in t),
#'  \item status = Tier 4 "overfished" status
#'  \item maxFofl = max allowed \eqn{F_{OFL} (=\gamma \cdot M)} [Tier 4]
#'  \item Fofl   = Tier 4 \eqn{F_{OFL}}, based on the Tier 4 harvest control rule
#'  \item retOFL = retained portion of total OFL (in t)
#'  \item dscOFL = discard portion of total (male + female) OFL (in t)
#'  \item OFL    = total OFL (in t)
#'}
#'
#'@export
#'
calcOFL<-function(mmbSrvCurr,
                  Bmsy,
                  theta,
                  M=0.18,
                  gamma=1.0,
                  alpha=0.1,
                  beta=0.25,
                  t.sf=3/12,
                  t.fm=4/12,
                  pct.male=0.5,
                  verbose=FALSE){
    #calc max Fofl
    maxFofl<-gamma*M;

    #find Fofl by iteration
    itF <- maxFofl; dF<-Inf; cnt<-0;
    while((abs(dF)>1.0e-4)&(cnt<100)){
        #calc projected MMB based on Fofl "guess"
        prjMMB<-calcPrjMMB(mmbSrvCurr,itF,theta,
                           M=M,t.sf=t.sf,t.fm=t.fm);
        #Fofl corresponding to projected MMB at mating based on "guessed" Fofl
        Fofl<-calcFofl(prjMMB$mmb,Bmsy,maxFofl,alpha,beta);
        #increment guess and counter
        dF  <- Fofl - itF;
        itF <- itF+0.2*dF;
        cnt<-cnt+1;
    }
    if (verbose) cat("iteration count: ",cnt,". Fofl: ",Fofl,"\n");

    #calculate projected MMB at Fofl
    prjMMB<-calcPrjMMB(mmbSrvCurr,Fofl,theta,
                       M=M,t.sf=t.sf,t.fm=t.fm,verbose=verbose);

    status<-ifelse(prjMMB$mmb/Bmsy<0.5,"overfished","not overfished");
    
    return(list(prjMMB=prjMMB$mmb,Bmsy=Bmsy,status=status,
                maxFofl=maxFofl,Fofl=Fofl,
                retOFL=prjMMB$retM,dscOFL=prjMMB$dscM/pct.male,
                OFL=prjMMB$retM+prjMMB$dscM/pct.male));
}


