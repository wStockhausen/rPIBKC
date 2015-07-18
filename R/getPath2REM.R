#'
#'@title Get the path to the ADMB random effects model (REM) used for survey averaging.
#'
#'@description Function to get the path to the ADMB random effects model (REM) used for survey averaging.
#'
#'@details Opens a file selector for the user to select the ADMB model.
#'
#'@return the path (as a string) to the ADMB model
#'
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
getPath2REM<-function(){
    fn<-selectFile(ext="*",caption="Select ADMB random effects/kalman filter model for survey averaging")
    return(fn);
}