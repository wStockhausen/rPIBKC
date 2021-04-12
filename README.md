# rPIBKC
R package for the PI blue king crab stock assessment.

##2019 assessment
The timing of the 2019 assessment was changed from September (after the summer NMFS EBS Trawl Survey) 
to May (before the survey), requiring the random walk smoothing model to *predict*, rather than estimate, 
the assessment-year survey biomass. Changes to the admb model output and the \code{surveyAveraging.RE} 
R package function have been made to accommodate this requirement.
