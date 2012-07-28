##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gprogressbar
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @seealso The documentation for this is found at \code{\link{gprogressbar}}.
##' @method .gprogressbar guiWidgetsToolkitQt
##' @S3method .gprogressbar guiWidgetsToolkitQt
.gprogressbar.guiWidgetsToolkitQt <- function(toolkit, value, container, ...) {
  GProgressBar$new(toolkit, value, container, ...)
}

##' For Qt, the Gprogressbar class has the extra reference method
##' \code{set_border}. The \code{border} argument has been deprecated.
##' @rdname gWidgets2Qt-package
GProgressBar <- setRefClass("GProgressBar",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit=NULL, value, container, ...) {
                                
                                widget <<- Qt$QProgressBar()
                                widget$setMaximum(100L)
                                widget$setMinimum(0L)

                                if(!missing(value))
                                  set_value(value)
                                
                                initFields(block=widget)
                                
                                add_to_parent(container, .self, ...)

                                callSuper(toolkit)
                              },
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                widget$setMaximum(if(is.null(value)) 0L else 100L)
                                if(!is.null(value))
                                  widget$setValue(as.integer(value))
                              },
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                widget$value()
                              }
                              ))


