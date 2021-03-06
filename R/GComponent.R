##' @include qt-misc.R
NULL


## Base classes. These are *not* exported, rather each toolkit implementation needs
## to (mostly) provide these same basic classes:
## GComponent
##   - GWidget
##     - GButton
##     - GLabel
##     - Others matching the constructors
##   -GContainer
##     - GWindow
##     - GGroup
##       - GFrame
##         - GExpandGroup
##     - GLayout
##     - GNotebook
##     - GPanedGroup
##     - GStacked


##' Base Class. We have
##' GComponent as parent for GContainer and GWidget.
##' Here we place QObject and QWidget values, although separating might be better
GComponent <- setRefClass("GComponent",
                               contains="BasicToolkitInterface",
                               fields=list(
                                 toolkit="ANY",
                                 widget="ANY",
                                 block="ANY",
                                 parent="ANY", # NULL for gwindow, else parent container
                                 handler_id="ANY",
                                 .e="environment",
                                 .invalid="logical",
                                 .invalid_reason="character"
                                 ),
                               methods=list(
                                 initialize=function(toolkit=guiToolkit(), ..., expand, fill, anchor, label, index, align) {
                                   initFields(toolkit=toolkit,
                                              .e=new.env()
                                              )
                                   if(is(handler_id, "unitializedField"))
                                     handler_id <<- NULL
                                   
                                   callSuper(...)
                                 },
                                 get_length = function(...) {
                                   "Get length of object. Needed for sapply."
                                   1
                                 },
                                 get_visible = function() widget$visible,
                                 set_visible = function(value) widget$setVisible(as.logical(value)),
                                 get_enabled = function() widget$enabled,
                                 set_enabled = function(value) widget$setEnabled(as.logical(value)),
                                 get_focus = function() widget$focus,
                                 set_focus = function(value) {
                                   if(value) widget$setFocus(Qt$Qt$OtherFocusReason)
                                 },
                                 set_font = function(value) {
                                   value <- as.list(value)
                                   ## qfont has values for family, pointsize, weight, italic with defaults given by baseFont$style()

                                   ## we just need to map
                                   vals <- list()
                                   if(!is.null(value$family))
                                     vals$family <- switch(value$family,
                                                           "sans"="Arial",
                                                           "helvetica"="Helvetica",
                                                           "times"="Times",
                                                           "monospace"="Courier",
                                                           NULL)

                                   
                                   if(!is.null(value$weight))
                                     vals$weight <- switch(value$weight,
                                                           "light"=Qt$QFont$Light,
                                                           "normal"=Qt$QFont$Normal,
                                                           "medium"=Qt$QFont$DemiBold,
                                                           "bold"=Qt$QFont$Bold,
                                                           "heavy"=Qt$QFont$Black,
                                                           NULL)

                                   if(!is.null(value$style)) 
                                     vals$italic <- value$styles == "italic"

                                   if(!is.null(value$size))
                                     vals$pointsize <- as.integer(value$size)


                                   if(!is.null(value$scale))
                                     vals$pointsize <- switch(value$scale,
                                                         "xx-large"=24,
                                                         "x-large"=18,
                                                         "large"=14,
                                                         "medium"=12,
                                                         "small"=10,
                                                         "x-small"=8,
                                                         "xx-small"=6,
                                                         NULL)

                                   fnt <- do.call(qfont, vals)
                                   if(!is.null(widget$font))
                                     widget$font <<- fnt

                                   ## colors??? /Users/verzani/pmg/GW-refactor/gWidgets2Qt/R/XXX
                                   ## set with stylesheet. Maybe better to use palette
                                   if(!is.null(value$color))
                                     widget$setStyleSheet(sprintf("* {color: %s}", value$color))
                                 },
                                 ## size, size<-
                                 get_size=function() {
                                   "Returns size hint. (Is there a better choice?)"
                                   c(width=block$width, height=block$height)
                                 },
                                 set_size=function(value) {
                                   ## value is possibly a list
                                   value <- unlist(value)
                                   value <- rep(value, length=2) # recycle if need be
                                   block$setMaximumSize(qsize(as.integer(value)))
                                 },
                                 ## tag
                                 get_attr = function(key) {
                                   if(missing(key))
                                     ls(.e)
                                   else
                                     attr(.e, key)
                                 },
                                 set_attr = function(key, value) {
                                   tmp <- .e
                                   attr(tmp, key) <- value
                                 },
                                 set_invalid=function(value, msg) {
                                   "Set widget as invalid or not"
                                   if(as.logical(value)) {
                                     .invalid <<- TRUE
                                     .invalid_reason <<- as.character(msg)
                                   } else {
                                     .invalid <<- FALSE
                                     .invalid_reason <<- ""
                                   }
                                 },
                                 is_invalid=function(...) {
                                   "Is widget in an invalid state"
                                   if(length(.invalid) == 0)
                                     .invalid <<- FALSE
                                   .invalid
                                 },
                                 ##
                                 set_parent = function(parent) parent <<- parent,
                                 add_to_parent = function(parent, child, expand=NULL, fill=NULL, anchor=NULL, ...) {
                                   "Add a child to parent if it is ia container and non null. Dispatches to add_child method of parent"
                                   if(missing(parent) || is.null(parent))
                                     return()
                                   if(!is(parent,  "GContainer")) {
                                     message("parent is not a container")
                                     return()
                                   }
                                   parent$add_child(child, expand, fill, anchor, ...)
                                 },
                                 ## Qt functions
                                 get_allocation=function() {
                                   "Return allocation (width x height) of widget"
                                   c(width=widget$width, height=widget$height)
                                 },
                                 is_extant=function() {
                                   !inherits(try(widget$parent(), silent=TRUE), "try-error")
                                 }
                                 ## ## Qt handler code
                                 ## handler_widget = function() widget, # allow override for block (glabel)
                                 ## add_handler = function(signal, handler, action=NULL, ...) {
                                 ##   if(is_empty(handler))
                                 ##     return(NULL)
                                 ##   ##
                                 ##   qconnect(handler_widget(), signal, handler, user.data=list(obj=.self, action=action))
                                 ## },
                                 ## add_event_handler=function(signal, handler, action=NULL, ...) {
                                 ##   XXX("implement (.addEventHandler)")
                                 ## },
                                 ## ## typical signal maps
                                 ## add_handler_clicked = function(handler, action=NULL, ...) {
                                 ##   add_handler("clicked", handler, action, ...)
                                 ## },
                                 ## add_handler_focus=function(handler, action=NULL, ...) {
                                 ##   add_event_handler("focusInEvent", handler, action, ...)
                                 ## },
                                 ## add_handler_blur=function(handler, action=NULL, ...) {
                                 ##   add_event_handler("focusOutEvent", handler, action, ...)
                                 ## },
                                 ## ##
                                 ## emit_signal=function(signal, ..., detail=NULL) {
                                 ##   "Emit signal, for svalue<- assignments, others"
                                 ##   XXX("qemit?")
                                 ## },
                                 ## ##
                                 ## block_handler=function(ID) {
                                 ##   ## we block all signals
                                 ##   if(!missing(ID))
                                 ##     message("All signals are blocked in Qt")
                                 ##   widget$blockSignals(TRUE)
                                 ## },
                                 ## unblock_handler=function(ID) {
                                 ##   widget$blockSignals(FALSE)
                                 ## },
                                 ## remove_handler=function(ID) {
                                 ##   if(!missing(ID)) {
                                 ##     message("Can only remove all handlers. Call without ID if that is desired")
                                 ##     return()
                                 ##   }
                                 ##   widget$disconnect()
                                 ## }

                                 )
                               )


##' GComponentObservable adds the observable interface
GComponentObservable <- setRefClass("GComponentObservable",
                                    fields=list(
                                      change_signal="character", # what signal is default change signal
                                      connected_signals="list"
                                      ),
                                    contains="GComponent",
                                    methods=list(
                                      ## Some decorators for handlers
                                      ## these wrap the handler to satisfy or fill the h object or return value
                                      event_decorator=function(handler) {
                                        "Decorator for basic event"
                                        force(handler)
                                        f <- function(e, .self, ...) {
                                          out <- handler(..., .self)
                                          if(is.atomic(out) && is.logical(out) && out[1])
                                            out[1]
                                          else
                                            FALSE # need logical
                                        }
                                        f
                                      },
                                      key_release_decorator=function(handler) {
                                        force(handler)
                                        f <- function(e, ...) {

                                          ## h$key <- event$getString() # XXX This is bad -- no locale, ...
                                          ## state <- gdkEventKeyGetState(event)
                                          ## if(state == 0)
                                          ##   h$modifier <- NULL
                                          ## else
                                          ##   h$modifier <- gsub("-mask", "", names(which(state == GdkModifierType)))
                                          handler(e,..., .self)
                                        }
                                        event_decorator(f)
                                      },
                                      button_press_decorator = function(handler) {
                                        "Add in position information to 'h' component"
                                        force(handler)
                                        f <- function(event, .self, ...) {
                                          ## stuff in some event information
                                          ## h$x <- event$getX(); h$X <- event$getXRoot()
                                          ## h$y <- event$getY(); h$Y <- event$getYRoot()
                                          ## h$state <- gsub("-mask", "", names(which(event$getState() == GdkModifierType)))
                                          ## h$button <- event$getButton()
                                          handler(widget, event, ..., .self)
                                        }
                                        event_decorator(f)
                                      },
                                      ## code for integrating observable interface with Qt
                                      handler_widget = function() widget, # allow override for block (e.g., glabel)
                                      is_handler=function(handler) {
                                        "Helper to see if handler is a handler"
                                        !missing(handler) && !is.null(handler) && is.function(handler)
                                      },
                                      ##
                                      ## Adding a handler means to
                                      ## a) create an observer and add an observer for the given signal
                                      ## 
                                      ## b) create a call back which
                                      ## calls the notify observer
                                      ## method when the widget
                                      ## actualy emits the signal
                                      add_handler=function(signal, handler, action=NULL, decorator,
                                        emitter=.self$handler_widget()) {
                                        "Uses Observable framework for events. Adds observer, then call connect signal method. Override last if done elsewhere"
                                        if(is_handler(handler)) {
                                          o <- gWidgets2:::observer(.self, handler, action)
                                          invisible(add_observer(o, signal))
                                          connect_to_toolkit_signal(signal, decorator, emitter=emitter)
                                        }
                                      },
                                      add_event_handler=function(handler, action=NULL, ..., decorator) {
                                        add_handler(handler, action=NULL, decorator=.self$event_decorator, ...)
                                      },
                                      

                                      connect_to_toolkit_signal=function(
                                        signal, # which signal (qconnect)
                                        decorator,
                                        emitter=.self$handler_widget() # can override here
                                        ) {
                                        "Connect signal of toolkit to notify observer"

                                        f <- function(...) {
                                          ## user.data is last value passed in
                                          l <- list(...);
                                          .self <- l[[length(l)]]
                                          .self$notify_observers(signal=signal, ...)
                                        }
                                        if(!missing(decorator))
                                          f <- decorator(f)
                                        
                                        ## only connect once
                                        if(is.null(connected_signals[[signal, exact=TRUE]]))
                                          out <- try(qconnect(emitter, signal, handler=f, user.data=.self), silent=TRUE)
                                        if(inherits(out, "try-error"))
                                          message("can't connect signal", signal)
                                        connected_signals[[signal]] <<- TRUE
                                      },
                                      ## initiate a handler (emit signal)
                                      invoke_handler=function(signal, ...) {
                                        "Bypasses gSignalEmit which crashes R for me.
                                        Invoke observers listening to signal"
                                        notify_observers(..., signal=signal)
                                      },
                                      invoke_change_handler=function(...) {
                                        "Generic change handler invoker."
                                        if(!is(change_signal, "uninitializedField") && length(change_signal))
                                          invoke_handler(signal=change_signal, ...)
                                      },
                                      ## block and unblock
                                      block_handlers=function() {
                                        "Block all handlers."
                                        ## default is to block the observers. 
                                        block_observers()
                                      },
                                      block_handler=function(ID) {
                                        "Block a handler by ID"
                                        block_observer(ID)
                                      },
                                      unblock_handlers=function() {
                                        "unblock blocked observer. May need to be called more than once to clear block"
                                        unblock_observers()
                                      },
                                      unblock_handler=function(ID) {
                                        "unblock a handler by ID"
                                        unblock_observer(ID)
                                      },
                                      remove_handlers=function() {
                                        "Remove all observers"
                                        remove_observers()
                                      }, 
                                      remove_handler=function(ID) {
                                        "remove a handler by ID"
                                        remove_observer(ID)
                                      },
                                      
                                      ## basic set of handlers
                                      add_handler_changed=function(handler, action=NULL,...) {
                                        if(!is(change_signal, "uninitializedField") && length(change_signal)) {
                                          add_handler(change_signal, handler, action, ...)
                                        } else {
                                          stop("No change_signal defined for widget")
                                        }
                                      },
                                      ## Defind add_handler_EVENT methods
                                      add_handler_keystroke=function(handler, action=NULL, ...) {
                                        "Keystroke handler. Defined for all, but might restrict to only gedit, gtext"
                                        message("XXX no keystroke handler defined for object of class", class(.self))
                                      },                                 
                                      add_handler_clicked = function(handler, action=NULL, ...) {
                                        add_handler("clicked", handler, action, ...)
                                      },
                                      add_handler_button_press=function(handler, action=NULL, ...) {
                                        message("XXX no keystroke handler defined for object of class", class(.self))
                                      },
                                      add_handler_focus=function(handler, action=NULL, ...) {
                                        message("XXX no focus handler defined for object of class", class(.self))
                                      },
                                      add_handler_blur=function(handler, action=NULL, ...) {
                                        message("XXX no focus out handler defined for object of class", class(.self))
                                      },
                                      ## DND
                                      add_drop_target=function(handler, action=NULL, ...) {
                                        "We use drop-event signal. This is not connected to a widget"
                                        widget$acceptDrops <<- TRUE
                                        if(is_handler(handler)) {
                                          o <- gWidgets2:::observer(.self, handler, action)
                                          invisible(add_observer(o, "drop-event"))
                                        }
                                      },
                                      add_drop_source=function(handler, action=NULL, ...) {
                                        "We use drag-event signal. Not connected to a widget"
                                        widget$dragEnabled <<- TRUE
                                        if(is_handler(handler)) {
                                          o <- gWidgets2:::observer(.self, handler, action)
                                          remove_observers("drag-event") # only 1
                                          invisible(add_observer(o, "drag-event"))
                                        }
                                      },
                                      ## some more
                                      add_popup_menu = function(mb, action=NULL, ...) {
                                      },
                                      add_3rd_mouse_popup_menu=function(mb, action=NULL, ...) {
                                        ## XXX need to do for Qt
                                        
                                        if(is(mb, "list")) 
                                          mb <- gmenu(mb, popup=TRUE)

                                        if(!is(mb, "GMenuPopup"))
                                          stop("Pass in popup menu or list defining one")

                                        widget$setContextMenuPolicy(Qt$Qt$CustomContextMenu)                                        
                                        
                                        handler <- function(pt) {
                                          ## This pops up in upper left corner
                                          ## might want to move down and over right to center?
                                          mb$widget$exec(widget$mapToGlobal(Qt$QPoint(0,0)))
                                        }
                                        
                                        qconnect(handler_widget(), "customContextMenuRequested", handler)
                                      }


                                      ))
