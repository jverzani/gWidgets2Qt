##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gslider
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gslider guiWidgetsToolkitQt
##' @S3method .gslider guiWidgetsToolkitQt
.gslider.guiWidgetsToolkitQt <-  function(toolkit,
                                             from = 0, to = 100, by = 1, value = from, horizontal = TRUE,
                                             handler = NULL, action = NULL, container = NULL, ... ) {
  GSlider$new(toolkit,
              from, to, by, value, horizontal,
              handler,action, container, ...)
}


## glider class
GSlider <- setRefClass("GSlider",
                       contains="GWidget",
                       fields=list(
                         items = "ANY"
                         ),
                       methods=list(
                         initialize=function(toolkit,
                           from, to, by, value, horizontal,
                           handler, action, container, ...) {
                           if(length(from) == 1)
                             x <- seq(from, to, by)
                           else
                             x <- from
                           x <- sort(unique(x))
                           items <<- x

                           if(as.logical(horizontal)) {
                             ## ENUMS
                             widget <<- Qt$QSlider()
                             widget$setOrientation(Qt$Qt$Horizontal)
                             widget$setTickPosition(Qt$QSlider$TicksBelow) 
                           } else {
                             widget <<- Qt$QSlider(Qt$Qt$Vertical)
                             widget$setTickPosition(Qt$QSlider$TicksLeft) 
                           }
                           
                           widget$setMinimum(1L)
                           widget$setMaximum(length(x))
                           widget$setPageStep(1+floor(length(x)/5))
                           widget$setSingleStep(1L)

                           ## show tooltip with value
                           qconnect(widget, "valueChanged", function(value) {
                             widget$setToolTip(as.character(get_items()[value]))
                           })
                           
                           set_value(value[1])
                           
                           initFields(block=widget,
                                      change_signal="valueChanged")
                           
                           add_to_parent(container, .self, ...)

                           handler_id <<- add_handler_changed(handler, action)
                           
                           callSuper(toolkit)
                         },
                         get_value=function(drop=TRUE, ...) {
                           items[get_index()]
                         },
                         set_value=function(value, drop=TRUE, ...) {
                           i <- pmatch(value, items)
                           set_index(i)
                         },
                         get_index = function(...) {
                           widget$value
                         },
                         set_index = function(value,...) {
                           if(!is_empty(value))
                             widget$setValue(value) # widget uses index 1, ..., n
                         },
                         get_items = function(i, ...) {
                           items
                         },
                         set_items = function(value, i, ...) {
                           cur <- get_value()
                           items <<- sort(unique(value))
                           widget$setMinimum(1L)
                           widget$setMaximum(length(value))
                           
                           set_value(cur)
                         }
                         ))

