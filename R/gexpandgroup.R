##' @include gframe.R
NULL

##' toolkit constructor
##'
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gexpandgroup guiWidgetsToolkitQt
##' @S3method .gexpandgroup guiWidgetsToolkitQt
.gexpandgroup.guiWidgetsToolkitQt <- function(toolkit,
                                                 text, markup,  horizontal=TRUE,
                                                 handler=NULL, action=NULL,
                                                 container=NULL, ...) {
  GExpandGroup$new(toolkit, text=text, markup=markup, horizontal=horizontal, handler=handler, action=action, container=container, ...)
}

## base class from gframe
GExpandGroup <- setRefClass("GExpandGroup",
                            contains="GBoxContainer",
                            fields=list(
                              "markup"="logical"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, text, markup=FALSE, horizontal=TRUE, handler, action, container=NULL, ..., expand=FALSE, fill=FALSE) {

                                ## XXX
                                stop("Need to implement this -- but there is no nice widget!")

                                
                                handler_id <<- add_handler_changed(handler, action)
                                add_to_parent(container, .self, expand, fill, ...)
                                
                                callSuper(toolkit, horizontal=horizontal, ...)
                              },
                              get_names=function(...) {
                                block$getLabel()
                              },
                              set_names=function(value, ...) {
                                block$setLabel(value)
                              },
                              get_visible = function() {
                                block$getExpanded()
                              },
                              set_visible = function(value) {
                                block$setExpanded(as.logical(value))
                              },
                              add_handler_changed=function(handler, action, ...) {
                                add_handler("activate", handler, action, ...)
                              }
                              ))
                            
