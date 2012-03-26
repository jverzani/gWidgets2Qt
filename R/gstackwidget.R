##' @include gnotebook.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gnotebook
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gstackwidget guiWidgetsToolkitQt
##' @S3method .gstackwidget guiWidgetsToolkitQt
.gstackwidget.guiWidgetsToolkitQt <-  function(toolkit,
                                                  container = NULL, ... ) {
  GStackWidget$new(toolkit,
                   container = container, ...)
}



GStackWidget <- setRefClass("GStackWidget",
                            contains="GCardContainer",
                            methods=list(
                              initialize=function(toolkit=NULL,
                                container=NULL, ...) {

                                block <<- widget <<- Qt$QStackedWidget()
                                
                                add_to_parent(container, .self, ...)
                                
                                callSuper(toolkit)
                              },
                              add_child=function(child,  index=NULL,  ...) {
                                "Similar to GNotebook's, but without label and close button code"
                                
                                qt_child <- getBlock(child)
                                
                                if(is.null(index))
                                  index <- get_length() + 1
                                index <- min(get_length() + 1, max(1, index))
                             
                             
                                ## add at index
                                widget$insertWidget(index-1, qt_child)

                                child_bookkeeping(child)
                              }
                              ))

