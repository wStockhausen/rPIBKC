#'
#'@title Calculate \eqn{\theta}, the average exploitation rate on MMB
#'
#'@description Function to calculate \eqn{\theta}, the average exploitation rate on MMB
#'
#'@param mmbLst - output from \code{\link{calcMMBMating}}
#'@param assYr  - assesssment year
#'@param n      - number of years to include in average
#'
#'@return 2-element vector with \eqn{\theta} calculated as
#'\enumerate{
#'  \item \eqn{\theta = \frac{\sum_y M_{F_{y}}}{\sum_y MMB_{{fishing}_{y}}}}        [ratio of averages]
#'  \item \eqn{\theta = \frac{1}{N}\sum_y \frac{M_{F_{y}}}{{MMB}_{{fishing}_{y}})}} [average of ratios]
#'}
#'where
#'\itemize{
#'  \item \emph{N} = number of years over which to average
#'  \item \eqn{M_{F_{y}}} = total fishing-related mortality on mature males in year \emph{y}
#'  \item \eqn{{MMB}_{{fishing}_{y}}} = MMB just prior to the (pulse) fishery
#'}
#'@export
#'
calcTheta<-function(mmbLst,assYr=2014,n=3){
    #theta is average of discard mortality for MMB to MMB at fishing (an exploitation rate)
    mmbFsh<-mmbLst$mmbFsh;  #MMB at time of fisheries
    dscTot<-mmbLst$dscM$tot;#total discard mortality on MMB

    #years to average over
    yrs<-as.character(assYr-(1:n));

    #ratio of averages
    theta1<-sum(dscTot[yrs])/sum(mmbFsh[yrs]);

    #average of  ratios
    rat<-dscTot/mmbFsh;
    theta2<-mean(rat[yrs]);

    return(c(theta1,theta2));
}