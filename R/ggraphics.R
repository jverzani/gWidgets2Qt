##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @param width width of device (pixels)
##' @param height height of device (pixels)
##' @param dpi dots per inch
##' @param ps pointsize
##' @inheritParams gWidgets2::gwidget
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .ggraphics guiWidgetsToolkitQt
##' @S3method .ggraphics guiWidgetsToolkitQt
.ggraphics.guiWidgetsToolkitQt <-  function(toolkit,
                                               width = dpi*6, height = dpi*6, dpi = 75, ps = 12,    
                                               handler = NULL,action = NULL, container = NULL, ... ) {
  GGraphics$new(toolkit,
                width=width, height=height, dpi=dpi, ps=ps,
                handler = handler,action = action, container = container, ...)
}

##################################################
## subclass for mouse events
##
## Most of this is lifted from the qtutils code
##
qsetClass("OurQGraphicsScene", Qt$QGraphicsScene, function(parent=NULL) {
  super(parent)
  ## internal properties
  this$lastClick <- NULL
  this$buttonPress <- FALSE
})

qsetProperty("object", OurQGraphicsScene)
qsetMethod("setObject", OurQGraphicsScene, function(obj) this$object <- obj)


## call the clickHandler if present
qsetMethod("mousePressEvent", OurQGraphicsScene, function(e) {
  
  this$lastClick <- e$buttonDownScenePos(e$button())
  this$buttonPress <- TRUE

  w <- this$width()           
  ht <- this$height()
  X <- this$lastClick$x()
  Y <- this$lastClick$y()
  x <- grconvertX(X/w, from="ndc", to="user")
  y <- grconvertY((ht-Y)/ht, from="ndc", to="user")

  object$notify_observers(signal="mousePressEvent", x=x, y=y, width=w, height=ht, X=X, Y=Y)

  super("mousePressEvent", e)
})


qsetMethod("mouseReleaseEvent", OurQGraphicsScene, function(e) {


  this$lastRelease <- e$scenePos()
  this$buttonPress <- FALSE

  w <- this$width()           
  ht <- this$height()
  ## start
  x1 <- this$lastClick$x()
  y1 <- this$lastClick$y()
  ## finish 
  x2 <- this$lastRelease$x()
  y2 <- this$lastRelease$y()
  X <- c(x1,x2); Y <- c(y1, y2)
  x <- sort(grconvertX(X/w, from="ndc", to="user"))
  y <- sort(grconvertY((ht-Y)/ht, from="ndc", to="user"))

  ## call if we moved more than 10 blocks (in pixels)
  if(abs(diff(X)) + abs(diff(Y)) > 10)
    object$notify_observers(signal="mouseReleaseEvent", x=x, y=y, width=w, height=ht, X=X, Y=Y)

  super("mouseReleaseEvent", e)
})


##################################################

## make a class for a graphics device. This allows
## one to write methods for mouse handlers etc.
## Had trouble using creategwClass, although that would have been preferable
qsetClass("QtDevice", Qt$QGraphicsView)

## set gWidgets object that this class is the widget of. (self reference)
qsetProperty("object", QtDevice)
qsetMethod("setObject", QtDevice, function(obj) this$object <- obj)

## set as current device
qsetMethod("devSet", QtDevice, function() dev.set(dev))


## raise on mouse click
##
qsetMethod("mousePressEvent", QtDevice, function(e) {
  devSet()
  super("mousePressEvent", e)
})


## only called for top-level windows
## doesn't work. How to close device when window is destroyed
qsetMethod("closeEvent", QtDevice, function(e) {
  dev.off(dev)
  e$accept()
})

## initialize the scene for the view
qsetMethod("initScene", QtDevice, function(width, height, pointsize, family="") {
  the_scene <- OurQGraphicsScene()
  the_scene$setObject(object)
  
  this$rscene <- qsceneDevice(width, height, pointsize, family, the_scene)
  this$setScene(rscene)

  ## properties
  this$dev <- dev.cur()
  this$clickHandler <- NULL
  this$clickAction <- NULL

  ## setup widget
  setDragMode(Qt$QGraphicsView$RubberBandDrag) # do rubber banding. call addHandlerChanged to get coords
  setContextMenuPolicy(Qt$Qt$ActionsContextMenu)

  addActions()
})

## add actions to device
qsetMethod("addActions", QtDevice, function() {
  zoominAct <- Qt$QAction("Zoom In", this)
  zoominAct$setShortcut(Qt$QKeySequence("Ctrl++"))
  qconnect(zoominAct, signal = "triggered", handler = function(checked) {
    this$scale(1.2, 1.2)
  })
  this$addAction(zoominAct)

  zoomoutAct <- Qt$QAction("Zoom Out", this)
  zoomoutAct$setShortcut(Qt$QKeySequence("Ctrl+-"))
  qconnect(zoomoutAct, signal = "triggered", handler = function(checked) {
    this$scale(1/1.2, 1/1.2)
  })
  this$addAction(zoomoutAct)

  printHandler <- function(full = TRUE) {
    printer <- Qt$QPrinter(Qt$QPrinter$HighResolution)
    rpaper <- getOption("papersize")
    if (is.null(rpaper)) 
      rpaper <- "A4"
    qtpaper <- names(Qt$QPrinter)
        usepaper <- qtpaper[match(tolower(rpaper), tolower(qtpaper))]
    if (is.na(usepaper)) 
      usepaper <- "A4"
    printer$setPageSize(Qt$QPrinter[[usepaper]])
    pd <- Qt$QPrintDialog(printer)
    acceptPrint <- pd$exec()
    if (acceptPrint) {
      painter <- Qt$QPainter()
      painter$begin(printer)
      if (full) 
        rscene$render(painter)
      else this$render(painter)
      painter$end()
    }
    pd$d
  }
  printAct <- Qt$QAction("Print", this)
  printAct$setShortcut(Qt$QKeySequence("Ctrl+P"))
  qconnect(printAct, signal = "triggered", handler = function(checked) {
    printHandler(TRUE)
  })
  this$addAction(printAct)

  printVisibleAct <- Qt$QAction("Print visible", this)
  qconnect(printVisibleAct, signal = "triggered", handler = function(checked) {
    printHandler(FALSE)
  })
  this$addAction(printVisibleAct)
  qtutils:::addImageExportAction(this)

})


## Main class, uses QtDevice as the widget where most all the work is done.
GGraphics <- setRefClass("GGraphics",
                         contains="GWidget",
                         fields=list(
                           device_number="numeric",
                           rubber_band="environment"
                           ),
                         methods=list(
                           initialize=function(toolkit=NULL,
                             width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12, 
                             handler=NULL, action=NULL, container=NULL, ...) {
                             

                             theArgs <- list(...)
                             family <- getWithDefault(theArgs$family, "")
            
                             widget <<- QtDevice()
                             widget$setObject(.self)

                             initFields(
                                        block=widget,
                                        change_signal="mouseReleaseEvent"
                                        )
                             
                             widget$initScene((width/dpi), (height/dpi), ps, family)
                             
                             add_to_parent(container, .self, ...)

                             handler_id <<- add_handler_changed(handler, action)
                             
                             callSuper(toolkit)
                           },
                           get_value=function( ...) {
                             
                           },
                           set_visible=function(value, ...) {
                             "Set as current device, raise"
                             if(value) {
                               widget$devSet()
                             }
                           },
                           set_value=function(value, ...) {
                             "Save figure to file specfied by value"
                             
                           },
                           ## handlers
                           add_handler_clicked=function(handler, action=NULL, ...) {
                             add_handler("mouseReleaseEvent", handler, action, ...)
                           },
                           ## override, connected to in class
                           connect_to_toolkit_signal=function(...) {}
                           ))

