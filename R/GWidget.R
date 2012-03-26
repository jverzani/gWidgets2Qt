##' @include GComponent.R
NULL

##' Base class for widget objects.
GWidget <- setRefClass("GWidget",
                       contains="GComponentObservable"

                       )


##' Create a Sub class of a QWidget class so that we can run virtual Events
##'
##' This is necessary for many widgets, though should be used
##' sparingly as it will be much slower -- things are looked up in R,
##' not C++ code These are used as follows: a) use
##' \code{createGQWClass} to create the class b) instantiate c) call
##' setObject to set reference class container.
##' @cname QWidget instance
##' @constructor function to pass to constructor in \code{qsetClass}
createGQWClass <- function(cname, constructor) {
  where <- parent.frame()               # what environment to define class


  ## this quiets R CMD
  super <- this <- NULL
  
  ## cname is QWidget
  newClassName <- sprintf("gw%s", cname)
  qtObj <- get(cname, envir=Qt)

  if(missing(constructor)) {
    constructor <- function(parent=NULL) {
      super(parent)
      this$data <- c()
      
      this$dndStartPosition <- NULL
      this$dropHandler <- NULL            # function(obj, data) data is passed
      this$dragHandler <- NULL            # function(obj, e) e is mouse event. Returns value to pass
    }
  }
  
  qsetClass(newClassName, qtObj, constructor, where=where)


  ## add methods
  NewClassObject <- get(newClassName)
  qsetProperty("object", NewClassObject)
  qsetMethod("setObject", NewClassObject, function(value) this$object <- value)
  ## handler stuff
  ##' FUnction call to run a handler
  ##' @param obj gWidgets object
  ##' @param eventName name of event
  ##' @param e mouse event passed in by Qt
  ##' @param components
  gwRunQtEventHandler <- function(obj, eventName, e, components=character()) {
    ## XXX do something with components -- sapply(component, function(i) e[[i]]) ??
    obj$notify_observers(eventName, e)
  }
    


  ## method to set the gWidgets Object, should do check
  ## basic flow is 2-step:
  ## e = gwQLineEdit()
  ## e$setObject(obj)
  ## qsetMethod("setObject", NewClassObject, function(obj) {
  ##   ## obj should be gWidget object -- check
  ##   this$data <- obj
  ## })

  ## ## method to get the gWidgets object
  ## qsetMethod("getObject", NewClassObject, function() {
  ##   this$data 
  ## })

  ## set an event handler
  ## called as follows:
  ## getWidget(obj)$setEventHandler("name", handler, action)
  qsetMethod("setEventHandler", NewClassObject, function(eventName, handler, action=NULL) {
    obj <- this$object
    obj$add_handler(eventName, handler, action=action)
  })

  qsetMethod("removeEventHandler", NewClassObject, function(id) {
    obj <- this$object
    obj$remove_handler(id)
  })

  ##' block an event handler
  ##' @param id id from adding event handler
  qsetMethod("blockEventHandler", NewClassObject, function(id) {
    obj <- this$object
    obj$block_handler(id)
    invisible()
  })

  ##' unblock an event handler
  ##' @param id id from addEventHandler
  qsetMethod("unblockEventHandler", NewClassObject, function(id) {
    obj <- this$object
    if(!missing(id))
      obj$unblock_handler(id)
    else
      obj$unblock_handlers()
    invisible()
  })


  ## The are various events that we have implemented
  ## we can pass in event values, see x,y below
  qsetMethod("mousePressEvent", NewClassObject, function(e) {
    if(!is.null(this$dragHandler)) {
      this$dndStartPosition <- e$pos()    # for drag and drop
    }
    
    obj <- this$object
    if(!is.null(obj)) {
      gwRunQtEventHandler(obj, "mousePressEvent", e, c("x","y"))
    }

    super("mousePressEvent", e)
  })

  ##' set a function for handling a drag event
  qsetMethod("setDragHandler", NewClassObject, function(f) {
    this$dragHandler <- f               # function(obj, e)
  })

  ##' set up drag and drop event if present
  qsetMethod("mouseMoveEvent", NewClassObject, function(e) {
    if(!is.null(this$dragHandler)) {
      if ((e$buttons() & Qt$Qt$LeftButton) && !is.null(this$dndStartPosition)) {
        dist <- (e$x() - this$dndStartPosition$x())^2 +  (e$y() - this$dndStartPosition$y())^2
        if (dist >= Qt$QApplication$startDragDistance()^2)
          this$prepareDrag(e)
      }
    }
    super("mouseMoveEvent", e)
  })

  
  ##' prepare drag event. Requires dragHandler
  qsetMethod("prepareDrag", NewClassObject, function(e) {
    if(!is.null(this$dragHandler)) {
      val <- this$dragHandler(this$object, e)
      md <- Qt$QMimeData()
      md$setData("R/serialized-data", serialize(val, NULL))

      drag <- Qt$QDrag(this)
      drag$setMimeData(md)
  
      drag$exec()
    }
  })

  ##' when we enter we change the color palette
  qsetMethod("dragEnterEvent", NewClassObject, function(e) {
    if(!is.null(this$dropHandler)) {
      this$setForegroundRole(Qt$QPalette$Dark)
      e$acceptProposedAction()
    }

    super("dragEnterEvent", e)
  })
  
  ##' when we leave we return the palette
  qsetMethod("dragLeaveEvent", NewClassObject, function(e) {
    if(is.null(this$dropHandler)) return()
    this$setForegroundRole(Qt$QPalette$WindowText)
    e$accept()

    super("dragLeaveEvent", e)    
  })

  ##' drop event calls dropHandler (set via setDropHandler)
  qsetMethod("dropEvent", NewClassObject, function(e) {
    if(!is.null(this$dropHandler)) {
      this$setForegroundRole(Qt$QPalette$WindowText)  
      md <- e$mimeData()
      if(md$hasFormat("R/serialized-data")) {
        data <- unserialize(md$data("R/serialized-data"))
        this$dropHandler(this$object, data)
        this$setBackgroundRole(Qt$QPalette$Window)
        e$acceptProposedAction()
      }
    }

    super("dropEvent", e)
  })
  
  ##' set a dropHandler. This implements drop area
  ##' f <- function(obj, value)
  qsetMethod("setDropHandler", NewClassObject, function(f) {
    this$setAcceptDrops(TRUE)
    this$dropHandler <- f
  })

  ##' Close Event
  ##' Called when a widget is given close signal from parent
  ##' call event ignore to kill
  ##' We use if the handler returns FALSE we call event ignorm
  ##' Handler must return FALSE to not close window. (Can't be closed if never returns
  qsetMethod("closeEvent", NewClassObject, function(e) {
    obj <- this$object
    if(!is.null(obj)) {
      out <- gwRunQtEventHandler(obj, "closeEvent", e)
      if(is.logical(out) && !out)
        e$ignore()
      else
        e$accept()
    }
    super("closeEvent", e)
  })

  ##' Focus events
  qsetMethod("focusInEvent", NewClassObject, function(e) {

      obj <- this$object
      if(!is.null(obj)) {
        gwRunQtEventHandler(obj, "focusInEvent", e)
      }

    super("focusInEvent", e)
  })

  NewClassObject
} 



## Some commonly used class
GQWidget <- createGQWClass("QWidget")
