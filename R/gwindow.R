##' @include GContainer.R
NULL


##' toolkit constructor for gwindow
##'
##' @export
##' @rdname gWidgetsQt-undocumented
.gwindow.guiWidgetsToolkitQt <- function(toolkit, title, visible, name, width, height, parent, handler, action,  ...) {
  GWindow$new(toolkit, title, visible, name, width, height, parent, handler, action,  ...)
}

GWindow <- setRefClass("GWindow",
                            contains="GContainer",
                            fields=list(
                              menubar_area="ANY",
                              toolbar_area="ANY",
                              infobar_area="ANY",
                              content_area="ANY",
                              statusbar_area="ANY"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, title="",  visible=TRUE, name=NULL, width=NULL, height=NULL,
                                parent=NULL, handler, action, ...) {

                                ## XXX Parent stuff ...
                                block <<- widget <<- Qt$QMainWindow()
                                set_value(title)
                                initFields(toolkit=toolkit
                                           )

                                ## process parent (make transient for, location, ....
                                ## size of widget ...
                                ## handler for window close

                                if(visible)
                                  set_visible(TRUE)

                                callSuper(...)
                              },
                              ## Widget methods
                              get_value = function(...) widget$windowTitle,
                              set_value = function(value, ...) widget$setWindowTitle(paste(value, collapse=" ")),

                              get_size = function() {
                                c(width=widget$size$width(), height=widget$size$height())
                              },
                              set_size = function(value) {
                                value <- as.integer(value)
                                widget$resize(value[1], value[2])
                              },
                              update_widget = function(...) {
                                ### implement me
                              },
                              set_visible = function(value) {
                                if(as.logical(value)[1]) {
                                  widget$show()
                                  set_focus(TRUE)
                                } else {
                                  widget$hide()
                                }
                                ## XX recurse into central widget?
                              },
                              set_focus = function(value) {
                                if(as.logical(value)[1]) {
                                  widget$raise()
                                  widget$activateWindow()
                                } 
                              },
                              dispose_window=function() {
                                "close window"
                                widget$close()
                                widget$setParent(NULL)
                              },
                              ##
                              ## add methods
                              add_child=function(child, ...) {
                                if(missing(child) || is.null(child))
                                  return()

                                if(is(child, "GStatusBar")) {
                                  add_statusbar(child)
                                } else if(is(child, "GMenuBar")) {
                                  add_menubar(child)
                                } else if(is(child, "GToolBar")) {
                                  add_toolbar(child)
                                } else {
                                  widget$setCentralWidget(getBlock(child))
                                  child$set_parent(.self)
                                  children <<- list(child)
                                }
                              },
                              remove_child=function(child) {
                                child$set_parent(NULL)
                                widget$remove(child)
                                children <<- list()
                              },
                              add_menubar=function(child, ...) {
                                widget$setMenuBar(getBlock(child))
                              },
                              add_toolbar=function(child, ...) {
                                widget$addToolBar(getBlock(child))
                              },
                              add_statusbar=function(child) {
                                widget$setStatusBar(getBlock(child))
                              },
                              set_infobar=function(msg, ...) {
                                XXX("infobar needs to be built in")
                                ## infobar something like grabs toplevel, the svalue methods calls this
                                ## infobar$getToplevel()$set_infobar(msg)
                              },
                              set_statusbar=function(msg, ...) {
                                widget$statusBar()$clearMessage()
                                widget$statusBar()$showMessage(msg)
                              },
                              clear_statusbar=function(msg, ...) {set_statusbar("")},
                              add_handler_unrealize = function(...) {XXX("Implement me")},
                              add_handler_destroy = function(...) {XXX("Implement me")}
                              ))


                              
