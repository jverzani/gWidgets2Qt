##' @include GWidget.R
##' @include icons.R
NULL


##' Toolkit button constructor
##'
##' @export
##' @rdname gWidgets2Qt-undocumented
.gbutton.guiWidgetsToolkitQt <- function(toolkit, text,  handler, action, container, ...) {
  if(is(action, "GAction"))
    GButtonAction$new(toolkit, action, container, ...)
  else
    GButtonBase$new(toolkit, text, handler, action, container, ...)
}

##' Button reference class
GButton <- setRefClass("GButton",
                            contains="GWidget",
                            fields=list(
                              other = "ANY"
                              ),
                            methods=list(
                              set_value=function(value, drop=TRUE, ...) {
                                if(missing(value))
                                  return()
                                widget$setText(value)

                                ## XXX Fill in when icons.R is ready
                                icon <- getStockIconByName(tolower(value))
                                widget$setIcon(Qt$QIcon())                                
                                if(!is.null(icon)) {
                                  if(is(icon, "QIcon"))
                                    widget$setIcon(icon)
                                  else if(is(icon, "QtEnum"))
                                    widget$setIcon(Qt$QApplication$style()$standardIcon(icon))
                                }
                              },
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                widget$text
                              },
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_clicked(handler, action=action, ...)
                              }
                              ))

GButtonBase <- setRefClass("GButtonBase",
                       contains="GButton",
                       methods=list(
                         
                         initialize=function(toolkit, text, handler, action, container, ...) {
                           widget <<- Qt$QPushButton()
                           initFields(block=widget)
                           
                           add_to_parent(container, .self, ...)
                           
                           
                           if(!missing(text))
                                  set_value(text)
                           
                           handler_id <<- add_handler_changed(handler, action)
                           
                           callSuper(toolkit, ...)
                         }
                         ))
                         
GButtonAction <- setRefClass("GButtonAction",
                             contains="GButton",
                             methods=list(
                               initialize=function(toolkit,  action, container, ...) {
                                 widget <<- Qt$QPushButton()
                                 initFields(block=widget)
                                 
                                  add_to_parent(container, .self)
                           
                                 a <- getWidget(action)
                                 widget$addAction(a)

                                 ## need to set up connection between b and action
                                 ## leave icon coming from text, could do otherwise
                                 set_value(a$text)

                                 qconnect(widget,"clicked", function() a$trigger())
                                 qconnect(a, "changed", function() {
                                   set_value(a$text)
                                   set_enabled(a$enabled)
                                 })

                                 callSuper(...)
                               }
                               ))
