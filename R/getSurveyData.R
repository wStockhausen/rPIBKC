#'
#'@title Read survey data (abundance, biomass) from a csv file and return it as a dataframe
#'
#'@description Function to read survey data (abundance, biomass by sex and category) 
#'from a csv file and return it as a dataframe.
#'
#'@param fn - survey data filename
#'
#'@return dataframe with same columns as input file. Input units are 
#'converted to t (biomass) and ones (abundance).
#'
#'@details Uses \code{wtsUtilities::selectFile()}. The input datafile must be a csv
#'file with columns 
#'\itemize{
#'  \item year
#'  \item type (abundance/biomass)
#'  \item sex
#'  \item category
#'  \item units
#'  \item value
#'  \item cv
#'}
#' Allowable units for abundance are 'ones', 'thousands', 'millions'.
#' Allowable units for biomass are 'kg', 't', and "1000's t".
#' 
#' @import wtsUtilities
#' 
#'@export
#'
getSurveyData<-function(fn=NULL){
    if (is.null(fn)){
        fn<-wtsUtilities::selectFile(ext="csv",caption="Select csv file with survey data")
        if (is.null(fn)) return(NULL);
    }
    srvData<-read.csv(fn,stringsAsFactors=FALSE);

    #scale survey data to biomass in t, abundance in ones
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