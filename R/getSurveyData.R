#'
#'@title Read the survey data from a csv file and return it as a dataframe.
#'
#'@description Function to read the survey data from a csv file and return it as a dataframe.
#'
#'@param fn - survey data filename
#'
#'@return dataframe with columns
#'\itemize{
#'  \item year
#'  \item type (abundance/biomass)
#'  \item sex
#'  \item category
#'  \item units
#'  \item value
#'  \item cv
#'\}
#' Input units are converted to 1000's t (biomass) and millions (abundance).
#'
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
getSurveyData<-function(fn=NULL){
    if (is.null(fn)){
        fn<-selectFile(ext="csv",caption="Select csv file with survey data")
        if (is.null(fn)) return(NULL);
    }
    srvData<-read.csv(fn,stringsAsFactors=FALSE);

    #scale survvey data to 1000's t, abundance in millions
    idx<-srvData$units=='t';
    srvData$value[idx]<-srvData$value[idx]/1000;#biomass in 1000's t
    srvData$units[idx]<-"1000's t";
    idx<-srvData$units=='ones';
    srvData$value[idx]<-srvData$value[idx]/1.0e6;#abundance in millions
    srvData$units[idx]<-"millions";

    return(srvData);
}