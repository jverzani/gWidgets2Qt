##' @include misc.R
NULL

## import getWidget, getBlock for gWidgets2 for this, we don't need to export. This is used internally only

##' method for stopping getWidget
##' 
##' @param obj an RQtObject
##' @export
getWidget.RQtObject <- function(obj) obj

##' S3 method for stopping getBlock
##'
##' @param obj an RQtObject
##' @export
getBlock.RQtObject <- function(obj) obj

