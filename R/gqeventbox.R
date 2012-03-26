##' @include GWidget.R
NULL



## need a widget class for event boxes
## See glabel for an implementation where we do mousePressEvent
qsetClass("GQEventBox", Qt$QWidget)

## We have one main property, the R5 object this belongs to.
qsetProperty("object", GQEventBox)
qsetMethod("setObject", GQEventBox, function(value) this$object <- value)


qsetMethod("mousePressEvent", GQEventBox, function(e) {
  ## Do we do things differently based on the button? e$button() is an enum

  ## might give error?
  obj <- this$object
  if(!is.null(obj))
    obj$notify_observers(signal="mouse-press-event")
  else
    warning("Object is null. DId you call setObject with a reference class instance?")
    
})



##################################################
## A widget box for event box with DND interface
##
qsetClass("GQEventBoxDND", GQEventBox, function(parent=NULL) {
  super(parent)

  this$data <- c()
  this$dndStartPosition <- NULL
  this$dragHandler <- NULL
  this$dropHandler <- NULL

})

## set up drag and drop event if present
qsetMethod("mouseMoveEvent", GQEventBoxDND, function(e) {
  if(!is.null(this$dragHandler)) {
    if ((e$buttons() & Qt$Qt$LeftButton) && !is.null(this$dndStartPosition)) {
      dist <- (e$x() - this$dndStartPosition$x())^2 +  (e$y() - this$dndStartPosition$y())^2
      if (dist >= Qt$QApplication$startDragDistance()^2)
        this$prepareDrag(e)
    }
  }
  super("mouseMoveEvent", e)
})

  


## prepare drag event. Requires dragHandler
qsetMethod("prepareDrag", GQEventBoxDND, function(e) {
  if(!is.null(this$dragHandler)) {
    val <- this$dragHandler(this$object, e)
    md <- Qt$QMimeData()
    md$setData("R/serialized-data", serialize(val, NULL))
    
    drag <- Qt$QDrag(this)
      drag$setMimeData(md)
    
    drag$exec()
  }
})


## when we enter we change the color palette
qsetMethod("dragEnterEvent", GQEventBoxDND, function(e) {
  if(!is.null(this$dropHandler)) {
    this$setForegroundRole(Qt$QPalette$Dark)
    e$acceptProposedAction()
  }
  
  super("dragEnterEvent", e)
})
  
## when we leave we return the palette
qsetMethod("dragLeaveEvent", GQEventBoxDND, function(e) {
  if(is.null(this$dropHandler)) return()
  this$setForegroundRole(Qt$QPalette$WindowText)
  e$accept()
  
  super("dragLeaveEvent", e)    
})

## drop event calls dropHandler (set via setDropHandler)
qsetMethod("dropEvent", GQEventBoxDND, function(e) {
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
  
## set a dropHandler. This implements drop area
## f <- function(obj, value)
qsetMethod("setDropHandler", GQEventBoxDND, function(f) {
  this$setAcceptDrops(TRUE)
  this$dropHandler <- f
})

qsetMethod("setDragHandler", GQEventBoxDND, function(f) {
  this$dragHandler <- f
})
