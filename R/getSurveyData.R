#'
#'@title Read the survey data from a csv file and return it as a dataframe
#'
#'@description Function to read the survey data from a csv file and return it as a dataframe.
#'
#'@param fn - survey data filename
#'
#'@return dataframe with columns:
#'\itemize{
#'  \item year
#'  \item type (abundance/biomass)
#'  \item sex
#'  \item category
#'  \item units
#'  \item value
#'  \item cv
#'}
#' Input units are converted to t (biomass) and ones (abundance).
#'
#'@details Uess \code{wtsUtilities::selectFile()}.
#'
#'@export
#'
getSurveyData<-function(fn=NULL){
    if (is.null(fn)){
        fn<-wtsUtilities::selectFile(ext="csv",caption="Select csv file with survey data")
        if (is.null(fn)) return(NULL);
    }
    srvData<-read.csv(fn,stringsAsFactors=FALSE);

    #scale survvey data to biomass in t, abundance in ones
    ###biomass in kg
    idx<-srvData$units=="kg";
    srvData$value[idx]<-srvData$value[idx]/1000;#biomass in t
    srvData$units[idx]<-"t";
    ###biomass in 1000's t
    idx<-srvData$units=="1000's t";
    srvData$value[idx]<-1000*srvData$value[idx];#biomass in t
    srvData$units[idx]<-"t";
    ###abundance in thousands
    idx<-srvData$units=='thousands';
    srvData$value[idx]<-1000*srvData$value[idx];#abundance in ones
    srvData$units[idx]<-"ones";
    ###abundance in millions
    idx<-srvData$units=='millions';
    srvData$value[idx]<-1.0e6*srvData$value[idx];#abundance in ones
    srvData$units[idx]<-"ones";

    return(srvData);
}