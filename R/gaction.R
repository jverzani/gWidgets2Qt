##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gaction
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gaction guiWidgetsToolkitQt
##' @S3method .gaction guiWidgetsToolkitQt
.gaction.guiWidgetsToolkitQt <-  function(toolkit,
                                             label, tooltip=NULL, icon = NULL, key.accel = NULL,
                                             handler = NULL,action = NULL, parent = NULL, ... ) {
  GAction$new(toolkit,
              label, tooltip=tooltip, icon = icon, key.accel = key.accel,
              handler = handler,action = action, parent = parent, ...)
}


## GAction class
GAction <- setRefClass("GAction",
                       contains="GWidget",
                       fields=list(
                         accel_key="ANY"
                         ),
                       methods=list(
                         initialize=function(toolkit=NULL,
                           label="", tooltip=NULL, icon = NULL, key.accel = NULL,
                           handler, action=NULL, parent, ...) {

                           if(is.null(parent))
                             qt_parent <- Qt$QWidget()
                           else
                             qt_parent <- getBlock(parent)
                           widget <<- Qt$QAction(label, qt_parent)

                           initFields(block=widget,
                                      accel_key=key.accel,
                                      change_signal="triggered"
                                      )


                           if(!is.null(tooltip))
                            set_tooltip(tooltip)

                           ## icon -- take for stock if not specified
                           if(is.null(icon))
                             icon <- label
                           icon <- getStockIconByName(icon)
                           widget$setIcon(as_qicon(icon))
                           
                           if(!is.null(key.accel)) {
                             ks <- Qt$QKeySequence(key.accel)
                             widget$setShortcut(ks)         # not working??
                           }

                           
                           add_handler_changed(handler, action)
                                  
                           callSuper(toolkit)
                         },
                         
                         get_value=function( ...) {
                           widget$text
                         },
                         set_value=function(value, ...) {
                           widget$setText(value)
                         },
                         set_tooltip=function(value, ...) {
                           widget$setToolTip(paste(value, "\n"))
                         }
                         ))

