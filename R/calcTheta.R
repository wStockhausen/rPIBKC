#'
#'@title Calculate theta, the average exploitation rate on MMB
#'
#'@description Function to calculate theta, the average exploitation rate on MMB
#'
#'@param mmbLst - output from calcMMBMating
#'@param assYr  - assesssment year
#'@param n      - number of years to average
#'
#'@return 2-element vector with theta calculated as
#'\enumerate{
#'  \item ratio of averages
#'  \item average of ratios
#'}
#'
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