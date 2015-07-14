#'
#'@title Calculate Fofl using the harvest control rule.
#'
#'@description Functio to calculate Fofl using the harvest control rule.
#'
#'@param mmb - MMB at mating
#'@param Bmsy - Bmsy
#'@param maxFofl - max allowed Fofl
#'@param alpha - x-intercept of sloping control line
#'@param beta - threshold mmb/Bmsy to allow a directed fishery
#'
#'@return the value of Fofl given the mmb/Bmsy ratio, using the control rule.
#'
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

