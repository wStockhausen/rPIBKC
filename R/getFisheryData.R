#'
#'@title Read the fishery data from a csv file and return it as a dataframe.
#'
#'@description Function to read the fishery data from a csv file and return it as a dataframe.
#'
#'@param fn - fishery data filename
#'
#'@return dataframe with columns
#' year, type (abundance/biomass), fishery, gear (pot/trawl), units, category, catch
#' Input units are converted to 1000's t (biomass) and millions (abundance).
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

    #convert fishery data to 1000's t's
    idx<-fshData$units=='million lbs';
    fshData$catch[idx]<-fshData$catch[idx]/(2.204624);#catch in 1000's t
    fshData$units[idx]<-"1000's t";
    idx<-fshData$units=='t';
    fshData$catch[idx]<-fshData$catch[idx]/1000;#catch in 1000's t
    fshData$units[idx]<-"1000's t";

    return(fshData)
}