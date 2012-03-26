##' @include GWidget.R
NULL

##' Toolkit XXX constructor
##'
##' @inheritParams gWidgets2::gcheckbox
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gcheckbox guiWidgetsToolkitQt
##' @S3method .gcheckbox guiWidgetsToolkitQt
.gcheckbox.guiWidgetsToolkitQt <- function(toolkit,
                                              text, checked = FALSE, use.togglebutton=FALSE, handler = NULL, action = NULL,
                                              container = NULL, ... ) {
  if(use.togglebutton)
    GToggleButton$new(toolkit,
                      text, checked, handler, action, container, ...)
  else
    GCheckbox$new(toolkit,
                  text, checked, handler, action, container, ...)
}

## Checkbox reference class
GCheckbox <- setRefClass("GCheckbox",
                         contains="GWidget",
                         methods=list(
                           initialize=function(toolkit=NULL,
                             text="", checked = FALSE,
                             handler = NULL, action = NULL,
                             container = NULL, ... ) {
                             
                             widget <<- Qt$QCheckBox()
                             initFields(block=widget
                                        )
                             set_items(text)
                             set_value(checked)
                             
                             add_to_parent(container, .self, ...)
                             handler_id <<- add_handler_changed(handler, action)
                             
                             callSuper(toolkit)
                           },
                           set_value=function(value, drop=TRUE, ...) {
                             widget$setChecked(as.logical(value))
                             ## invoke_change_handler() # not needed?
                           },
                           get_value=function(index=TRUE, drop=TRUE, ...) {
                             state <- widget$checkState()
                             if(state == 2)
                               return(TRUE)
                             else if(state == 0)
                               return(FALSE)
                             else
                               return(NULL)
                           },
                           set_index=function(value, ...) {
                             "use 0 or 1 as index to convert to logical?"
                             set_value(as.logical(value))
                           },
                           get_index=function(value) {
                             as.numeric(get_value)
                           },
                           get_items = function(i, j, ..., drop=TRUE) {
                             widget$text
                           },
                           set_items = function(value, i, j, ...) {
                             widget$setText(format(value[1]))
                           },
                           get_names=function(...) get_items(),
                           set_names=function(value, ...) set_items(value),
                           add_handler_changed=function(...) add_handler_clicked(...),
                           add_handler_clicked=function(handler, action=NULL, ...) {
                             decorator <- function(cback) {
                               f <- function(state, .self) {
                                 cback(.self, state=state)
                               }
                             }
                             add_handler("stateChanged", handler, action, ...)
                           }
                           ))


## Basic toggle button class
GToggleButton <- setRefClass("GToggleButton",
                             contains="GWidget",
                             methods=list(
                               initialize=function(toolkit=NULL,
                                 text, checked = FALSE,  handler = NULL, action = NULL,
                                 container = NULL, ... ) {


                                 widget <<- Qt$QToolButton()
                                 widget$setCheckable(TRUE)

                                 set_items(value=text)
                                 set_value(checked)
                                 
                                 initFields(
                                            block=widget,
                                            change_signal="toggled"
                                           )
                                 
                                 add_to_parent(container, .self, ...)
                                 handler_id <<- add_handler_changed(handler, action)
                                 
                                 callSuper(toolkit)
                               },
                               get_value=function(...) widget$checked,
                               set_value=function(value, ...) widget$setChecked(as.logical(value)),
                               get_names=function(...) get_items(),
                               set_names=function(value, ...) set_items(value),
                              get_items = function(i, j, ..., drop=TRUE) {
                                widget$text
                              },
                              set_items = function(value, i, j, ...) {
                                ## use UseStock if in stock icon
                                widget$setText(value)
                              }
                              ))

