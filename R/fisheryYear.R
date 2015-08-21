#'
#'@title Returns the crab fishery year as a character string.
#'
#'@description Function to generate a character string representing a crab fishery year.
#'
#'@param year - the year (as of July 1) of the fishery
#'
#'@return the fishery year as a character string. For 2014, this would be '2014/15'
#'
#'@export
#'
fisheryYear<-function(year){
    str<-paste(year,'/',(year+1)%%100,sep='');
    return(str);
}