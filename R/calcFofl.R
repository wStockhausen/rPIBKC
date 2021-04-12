#'
#'@title Calculate \eqn{F_{OFL}} using the Tier 4 harvest control rule
#'
#'@description Functio to calculate \eqn{F_{OFL}} using the Tier 4 harvest control rule.
#'
#'@param mmb - MMB at mating (from calcMMBmating())
#'@param Bmsy - value for \eqn{B_{MSY}} (from clcBmsy())
#'@param maxFofl - max allowed \eqn{F_{OFL} (= \gamma M)}
#'@param alpha - value for the Tier 4 \eqn{\alpha} constant (the x-intercept of the sloping control rule)
#'@param beta - value for the Tier 4 \eqn{\beta} constant (the threshold for \eqn{MMB/B_{MSY}} to allow directed fishing)
#'
#'@return the value of \eqn{F_{OFL}} given the mmb/Bmsy ratio, using the Tier 4 control rule.
#'
#'@details none.
#'@export
#'
calcFofl<-function(mmb,Bmsy,maxFofl,alpha,beta){
    ratMMB<-mmb/Bmsy;
    if (ratMMB>1){
        Fofl<-maxFofl;
    } else if (ratMMB<=beta){
        Fofl<-0;#RETAINED Fofl, not total Fofl
    } else {
        Fofl<-maxFofl*(ratMMB-alpha)/(1.0-alpha);
    }
    return(Fofl);
}

