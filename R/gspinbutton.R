##' @include GWidget.R
NULL

##' Toolkit XXX constructor
##'
##' @param digits digits
##' @inheritParams gWidgets2::gslider
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gspinbutton guiWidgetsToolkitQt
##' @S3method .gspinbutton guiWidgetsToolkitQt
.gspinbutton.guiWidgetsToolkitQt <-  function(toolkit,
                                                 from = 0, to = 10, by = 1, value = from, digits = 0,
                                                 handler = NULL,action = NULL, container = NULL, ... ) {
  GSpinButton$new( toolkit, from , to , by, value, digits,
                  handler = handler, action = action, container = container, ...)
}


## spingbutton class
GSpinButton <- setRefClass("GSpinButton",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit,
                                from = 0, to = 10, by = 1, value = from, digits = 0,
                                handler, action, container, ...) {


                                ## coerce to integer if meant to be
                                if(isTRUE(all.equal(from, as.integer(from))))
                                  from <- as.integer(from)
                                if(isTRUE(all.equal(to, as.integer(to))))
                                  to <- as.integer(to)
                                if(isTRUE(all.equal(by, as.integer(by))))
                                  by <- as.integer(by)
                                
                                if(is.integer(by) && is.integer(from))
                                  widget <<- Qt$QSpinBox()
                                else
                                  widget <<- Qt$QDoubleSpinBox()

                                widget$setMinimum(from)
                                widget$setMaximum(to)
                                widget$setSingleStep(by)
                                widget$setWrapping(TRUE)        # wrap around
                                


                                set_value(value)
                                
                                initFields(block=widget,
                                           change_signal="valueChanged"
                                           )

                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              get_value=function(drop=TRUE, ...) {
                                widget$value
                              },
                              set_value=function(value, drop=TRUE, ...) {
                                widget$setValue(value)
                              },
                              set_items = function(value, i, ...) {
                                ## check that value is a regular sequence
                                ## check that value is a regular sequence

                                if(length(value) <=1) {
                                  warning(gettext("Can only assign a vector with equal steps, as produced by seq"))
                                  return()
                                }
                                if(length(value) > 2 &&
                                   !isTRUE(all.equal(diff(diff(value)), rep(0, length(value) - 2)))) {
                                  warning(gettext("Can only assign a vector with equal steps, as produced by seq"))
                                  return()
                                }
                                ## get current value, increment
                                curValue <- get_value()
                                inc <- head(diff(value), n=1)

                                widget$setMinimum(min(value))
                                widget$setMaximum(max(value))
                                widget$setSingleStep(inc)
                                widget$setValue(curValue)
                         
                              }
                              ))

