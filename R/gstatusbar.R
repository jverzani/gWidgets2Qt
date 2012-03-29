##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::glabel
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gstatusbar guiWidgetsToolkitQt
##' @S3method .gstatusbar guiWidgetsToolkitQt
.gstatusbar.guiWidgetsToolkitQt <-  function(toolkit,
                                                text="",
                                                container = NULL, ... ) {
  GStatusBar$new(toolkit,
                 text=text,
                 container = container, ...)
}


GStatusBar <- setRefClass("GStatusBar",
                          contains="GContainer",
                          methods=list(
                            initialize=function(toolkit=NULL,
                              text="", container=NULL, ...) {
                              
                              widget <<- Qt$QStatusBar()
                              block <<- widget
                              
                              set_value(text)

                              if(!is.null(container))
                                if(is(container, "GWindow"))
                                  container$add_statusbar(widget)
                                else
                                  getTopLevel(container)$add_statusbar(widget)

                              callSuper(toolkit)
                              },
                              get_value=function( ...) {
                                widget$getLabel()
                              },
                              set_value=function(value, ...) {
                                 widget$clearMessage()
                                 widget$showMessage(paste(value, collapse="; "))
                              },
                            add_child=function(child, expand=FALSE, fill=NULL, anchor=NULL, ...) {

                              qt_child <- getBlock(child)
                              if(!is(qt_child,"QWidget")) return()
                              
                              widget$addPermanentWidget(qt_child)
                              
                              ## Internal bookkeeping, add to lists
                              if(is(child, "GComponent"))
                                child$set_parent(.self)
                                 children <<- c(children, child)
                            }
                            ))

