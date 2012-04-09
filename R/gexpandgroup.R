##' @include gframe.R
NULL

## TODO: on hide, can we update geometry of parent to collapse to necessary size?


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
                              trigger="ANY",
                              label="ANY",
                              inner="ANY",
                              is_open="logical",
                              "markup"="logical"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, text="", markup=FALSE, horizontal=TRUE, handler, action, container=NULL, ..., expand=TRUE, fill=TRUE) {

                                
                                widget <<- make_widget(horizontal)
                                block <<- make_block()
                                qconnect(trigger, "clicked", function() {
                                  .self$toggle_visible()
                                })

                                initFields(change_signal="toggled", # none really
                                           is_open=TRUE,
                                           markup=markup # ignored
                                           )
                                set_names(text)
                                set_visible(TRUE)

                                handler_id <<- add_handler_changed(handler, action)
                                add_to_parent(container, .self, expand, fill, ...)

                                callSuper(toolkit, horizontal=horizontal, ...)
                              },
                              make_block=function() {
                                "Make enclosing block"
                                ## Now put into block (widget is a layout)
                                ## Be careful to explicitly parent those objects not stored in the environment
                                align <- Qt$Qt$AlignLeft & Qt$Qt$AlignTop
                                block <<- Qt$QWidget()
                                
                                lyt <- Qt$QGridLayout(block) # add parent
                                block$setLayout(lyt)
                                
                                trigger <<- Qt$QPushButton()
                                label <<- Qt$QLabel("")
                                inner <<- Qt$QWidget()

                                inner$setLayout(widget)
                                
                                lyt$addWidget(trigger, 0, 0, align)
                                lyt$addWidget(label, 0, 1, align)
                                lyt$addWidget(inner, 1, 0, 1, 2, align)
                                
                                ## don't stretch button
                                lyt$setRowStretch(0, 0L)
                                lyt$setRowStretch(1, 1L)
                                lyt$setColumnStretch(0, 0L)
                                lyt$setColumnStretch(1, 1L)

                                block
                              },
                              get_visible=function() {
                                is_open
                              },
                              set_visible=function(value) {
                                if(missing(value))
                                  value <- !is_open # toggle

                                if(value) {
                                  inner$show()
                                  trigger$setIcon(as_qicon(Qt$QStyle$SP_TitleBarShadeButton))
                                } else {
                                  inner$hide()
                                  trigger$setIcon(as_qicon(Qt$QStyle$SP_TitleBarUnshadeButton))
                                  block$update()
                                  block$updateGeometry()
                                }
                                ## update geometry?
                                block
                                is_open <<- value
                                invoke_change_handler()
                              },
                              toggle_visible=function() set_visible(!is_open),
                              get_names=function(...) {
                                label$text
                              },
                              set_names=function(value, ...) {
                                if(is.null(value))
                                  value <- ""
                                label$setText(value)
                              },
                              ## override this
                              connect_to_toolkit_signal=function(...) {}
                              ))
                            
