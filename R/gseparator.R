##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::ggroup
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gseparator guiWidgetsToolkitQt
##' @S3method .gseparator guiWidgetsToolkitQt
.gseparator.guiWidgetsToolkitQt <-  function(toolkit,
                                         horizontal = TRUE,
                   container = NULL, ... ) {
  GSeparator$new(toolkit, horizontal=horizontal, container = container, ...)
}


GSeparator <- setRefClass("GSeparator",
                          contains="GWidget",
                          methods=list(
                            initialize=function(toolkit,
                              horizontal=TRUE, container=NULL,
                              ...) {

                              
                              widget <<- Qt$QFrame()
                              widget$setFrameShape(ifelse(horizontal, Qt$QFrame$HLine, Qt$QFrame$VLine))
                              widget$setFrameShadow(Qt$QFrame$Sunken)   
                              
                              initFields(block=widget)
                              
                              add_to_parent(container, .self, ...)
                              
                              callSuper(toolkit)
                            }
                            ))

