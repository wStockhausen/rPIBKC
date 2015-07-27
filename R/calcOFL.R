#'
#'@title Calculate the OFL using a Tier 4 approach.
#'
#'@description Function to calculate the OFL using a Tier 4 approach.
#'
#'@param mmbSrvCurr - "current" MMB at time of survey
#'@param Bmsy - Bmsy (see calcBmsy())
#'@param theta - theta (see calcTheta())
#'@param M - natural mortality rate
#'@param gamma - gamma for Tier 4 F_{ofl_{max}}
#'@param alpha - intercept (as fraction of Bmsy) for sloping portion of control rule
#'@param beta -  minimum stock size (as fraction of Bmsy) at which directed fishing may occur
#'@param t.sf - time (fraction of year) from survey to fishery
#'@param t.fm - time (as fraction of year) from fishery to mating
#'
#'@return List with elements:
#'\itemize{
#'  \item maxFofl = max allowed Fofl (=gamma*M)
#'  \item Bmsy   = Bmsy
#'  \item Fofl   = Fofl
#'  \item prjMMB = projected MMB
#'  \item retOFL = retained portion of total OFL
#'  \item dscOFL = discard portion of total OFL
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
                  t.fm=4/12){
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
        itF <- itF+dF;
        cnt<-cnt+1;
    }
    cat("iteration count: ",cnt,". Fofl: ",Fofl,"\n");

    #calculate projected MMB at Fofl
    prjMMB<-calcPrjMMB(mmbSrvCurr,Fofl,theta,
                       M=M,t.sf=t.sf,t.fm=t.fm);

    return(list(maxFofl=maxFofl,Bmsy=Bmsy,
                Fofl=Fofl,prjMMB=prjMMB$mmb,
                retOFL=prjMMB$retM,dscOFL=prjMMB$dscM))
}


