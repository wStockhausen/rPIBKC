#'
#'@title Calculate Bmsy by averaging over specified time intervals
#'
#'@description Function to calculate Bmsy by averaging over specified time intervals
#'
#'@param mmb - time series of mmb at mating to average
#'@param years - years to incude in average
#'
#'@return Bmsy (in 1000's t)
#'
#'@export
#'
calcBmsy<-function(mmb,years=c(1980:1984,1990:1997)){
    Bmsy<-mean(mmb[as.character(years)]);
    return(Bmsy);
}