##' @include gqeventbox.R
NULL

##' Toolkit label constructor
##'
##' @export
##' @rdname gWidgets2Qt-undocumented
.glabel.guiWidgetsToolkitQt <- function(toolkit, text="", markup=FALSE, editable=FALSE,
                                           handler=NULL, action=NULL, container=NULL,
                                           ...) {
  GLabel$new(toolkit, text, markup, editable, handler, action, container, ...)
}
  



##' label class
GLabel <- setRefClass("GLabel",
                            contains="GWidget",
                            fields=list(
                              markup="ANY",
                              editable="logical",
                              edit_widget = "ANY",
                              state="character"
                              ),
                            methods=list(

                              
                              initialize=function(toolkit, text, markup=FALSE, editable=FALSE,
                                handler, action, container, ...) {

                                ## We use the GQEventBox to capture
                                ## mouse events. This allows us to have an editable
                                ## box
                                initFields(widget=Qt$QLabel(),
                                           block=GQEventBox())
                                block$setObject(.self)
                                
                                if(editable) {
                                  ## Set up widget to toggle between
                                  state <<- "label"
                                  edit_widget <<- Qt$QLineEdit()
                                  qconnect(edit_widget, "returnPressed", function(e) { ## Need QLineEdit signal
                                    show_label_widget()
                                  })
                                  handler <- function(h, ...) {
                                    ifelse(state == "label", show_edit_widget(), show_label_widget())
                                  }
                                } 

                                block_lyt <- Qt$QHBoxLayout()
                                block_lyt$addWidget(widget)
                                block$setLayout(block_lyt)

                                
                                add_to_parent(container, .self, ...)

                                set_value(text)

                                ## XXX What is changed handler here for pressing value
                                ## is changed called on svalue, in which case we need also click
                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              ## set the value
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                value <- paste(value, collapse="\n")
                                widget$setText(value)
                              },
                              ## tricky part is for markup
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                widget$text
                              },

                              ## methods for editing
                              show_edit_widget = function() {
                                edit_widget$setText(get_value())
                                block$layout()$removeWidget(widget)
                                widget$setParent(NULL)
                                block$layout()$addWidget(edit_widget) 
                                state <<- "edit"
                              },
                              show_label_widget = function() {
                                set_value(edit_widget$text)
                                block$layout()$removeWidget(edit_widget)
                                edit_widget$setParent(NULL)
                                block$layout()$addWidget(widget)
                                state <<- "label"
                              },
                              ## Handler
                              handler_widget = function() block, # put on block,not widget
                              connect_to_toolkit_signal=function(...) {}, # noop
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_clicked(handler, action=action, ...)
                              },
                              add_handler_clicked=function(handler, action=NULL, ...) {
                                add_handler("mouse-press-event", handler, action, ...)
                              }
                              ))

