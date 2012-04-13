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
                          fields=list(
                            msg="character"
                            ),
                          methods=list(
                            initialize=function(toolkit=NULL,
                              text="", container=NULL, ...) {
                              
                              widget <<- Qt$QStatusBar()

                              initFields(block = widget,
                                         msg="")
                              
                              set_value(text)

                              if(!is.null(container))
                                if(is(container, "GWindow"))
                                  container$add_statusbar(widget)
                                else
                                  getTopLevel(container)$add_statusbar(widget)

                              callSuper(toolkit)
                              },
                              get_value=function(...) {
                                msg
                              },
                              set_value=function(value, ..., timeout=0L) {
                                "Show message. Can make transient by passing timeout=secs (0L for permanent)"
                                 widget$clearMessage()
                                 msg <<- paste(value, collapse="; ")
                                 widget$showMessage(msg, as.integer(timeout)*1000) # ms for widget, sec for interface
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

