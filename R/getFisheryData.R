#'
#'@title Read the fishery data from a csv file and return it as a dataframe.
#'
#'@description Function to read the fishery data from a csv file and return it as a dataframe.
#'
#'@param fn - fishery data filename
#'
#'@return dataframe with columns
#'\itemize{
#'  \item year
#'  \item type (abundance/biomass)
#'  \item fishery
#'  \item gear (pot/trawl)
#'  \item units
#'  \item category
#'  \item catch
#'}
#' Input units are converted to t (biomass) and millions (abundance).
#'
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
getFisheryData<-function(fn=NULL){
    if (is.null(fn)){
        fn<-selectFile(ext="csv",caption="Select csv file with fishery data")
        if (is.null(fn)) return(NULL);
    }
    fshData<-read.csv(fn,stringsAsFactors=FALSE);

    #convert fishery data to t
    ###millions lbs to t
    idx<-fshData$units=='million lbs';
    fshData$catch[idx]<-1000*fshData$catch[idx]/(2.204624);#catch in t
    fshData$units[idx]<-"t";
    ###1000's t to t
    idx<-fshData$units=="1000's t";
    fshData$catch[idx]<-1000*fshData$catch[idx];#catch in t
    fshData$units[idx]<-"t";
    ###kg to t
    idx<-fshData$units=="kg";
    fshData$catch[idx]<-fshData$catch[idx]/1000;#catch in t
    fshData$units[idx]<-"t";

    return(fshData)
}