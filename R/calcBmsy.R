#'
#'@title Calculate \eqn{B_{MSY}} by averaging over specified time interval.
#'
#'@description Function to calculate \eqn{B_{MSY}} by averaging over specified time interval.
#'
#'@param mmb - time series of mmb at mating to average
#'@param years - years to incude in average
#'
#'@return \eqn{B_{MSY}} (in t)
#'
#'@export
#'
calcBmsy<-function(mmb,years=c(1980:1984,1990:1997)){
    Bmsy<-mean(mmb[as.character(years)]);
    return(Bmsy);
}