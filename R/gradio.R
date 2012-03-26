##' @include gcheckboxgroup.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gradio
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gradio guiWidgetsToolkitQt
##' @S3method .gradio guiWidgetsToolkitQt
.gradio.guiWidgetsToolkitQt <-  function(toolkit,
                                            items,selected=1, horizontal=FALSE, handler=NULL,
                                            action=NULL, container=NULL, ...
                                            ) {

  GRadio$new(toolkit, items, selected, horizontal,
             handler, action, container, ...)
}


## radio button class
GRadio <- setRefClass("GRadio",
                      contains="GButtonGroupWidget",
                      methods=list(
                        initialize=function(toolkit, items, selected, horizontal,
                          handler, action, container, ...) {

                          
                          if(horizontal)
                            widget <<-  Qt$QHBoxLayout()
                          else
                            widget <<- Qt$QVBoxLayout()
                          block <<- Qt$QWidget()
                          block$setLayout(widget)
                          
                          constructor <<- Qt$QRadioButton
                          
                          button_group <<- Qt$QButtonGroup()
                          button_group$setExclusive(TRUE)
                          
                          set_items(value=items)
                          set_index(selected)
                          add_to_parent(container, .self, ...)
                          
                          handler_id <<- add_handler_changed(handler, action)
                          
                          callSuper(toolkit)

                        },
                        get_index=function(...) {
                          btns <- button_group$buttons()
                          which(sapply(btns, function(i) i$checked))
                        },
                        set_index=function(value, ...) {
                          if(is.logical(value))
                            value <- which(value) # numeric now
                          btns <- button_group$buttons()
                          btns[[value]]$setChecked(TRUE)
                        }
                        ))

