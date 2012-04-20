##' @include GWidget.R
NULL


QtPredefinedIcons <- c("question"=Qt$QMessageBox$Question,
                       "info"=Qt$QMessageBox$Information,
                       "warning"=Qt$QMessageBox$Warning,
                       "error"=Qt$QMessageBox$Critical
                       )

##' toolkit implementation for gmessage
##'
##' @inheritParams gWidgets2::ginput
##' @return NULL
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gmessage guiWidgetsToolkitQt
##' @S3method .gmessage guiWidgetsToolkitQt
.gmessage.guiWidgetsToolkitQt <- function(toolkit,
                                             msg,
                                             title = "message",
                                             icon = c("info","warning","error","question"),
                                             parent=NULL,
                                             ...
                                             ) {

  
  if(!is.null(parent)) {
    toplevel <- getTopLevel(parent)
    parentw <- getBlock(toplevel)
    mb <- Qt$QMessageBox(parentw)
  } else {
    mb <- Qt$QMessageBox()
  }
  
  
  
  
  
  mb$setWindowTitle(title)
  mb$setText(msg[1])
  if(length(msg) >= 2)
    mb$setInformativeText(msg[2])
  
  icon = match.arg(icon)
  mb$setIcon(QtPredefinedIcons[icon])
  
  mb$setStandardButtons(Qt$QMessageBox$Ok) 
  
  ret <- mb$exec()
  return(TRUE)
            
}


##' toolkit implementation for gconfirm
##'
##' @inheritParams gWidgets2::ginput
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gconfirm guiWidgetsToolkitQt
##' @S3method .gconfirm guiWidgetsToolkitQt
.gconfirm.guiWidgetsToolkitQt <-  function(toolkit,
                                              msg,
                                              title = "Confirm",
                                              icon = c("info","warning","error","question"),
                                              parent=NULL,
                                              ...
                                              ) {

  
  if(!is.null(parent)) {
    toplevel <- getTopLevel(parent)
    parentw <- getBlock(toplevel)
    mb <- Qt$QMessageBox(parentw)
  } else {
    mb <- Qt$QMessageBox()
  }
  
            
  mb$setWindowTitle(title)
  mb$setText(msg[1])
  if(length(msg) >= 2)
    mb$setInformativeText(msg[2])
  
  icon = match.arg(icon)
  mb$setIcon(QtPredefinedIcons[icon])
  
  mb$setStandardButtons(Qt$QMessageBox$Ok | Qt$QMessageBox$Cancel)
  
  ret <- mb$exec()
  if(ret == 1024 ) {
    return(TRUE)
  } else {
    ## cancel
    return(FALSE)
  }
  
  
}


##' toolkit implmentation of ginput
##'
##' @inheritParams gWidgets2::ginput
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .ginput guiWidgetsToolkitQt
##' @S3method .ginput guiWidgetsToolkitQt
.ginput.guiWidgetsToolkitQt <- function(toolkit,
                                           msg,
                                           text="",
                                           title = "Input",
                                           icon = c("info","warning","error","question"),
                                           parent=NULL,                   
                                           ...
                                           ) {

  if(is.null(parent))
    parent <- Qt$QWidget()
  
  out <- Qt$QInputDialog$getText(getBlock(parent), title, msg, Qt$QLineEdit$Normal, text)
  
  ## out is NULL or a text
  if(is.null(out))
    out <- character(0)

  out

  
}         


##' toolkit implementation
##'
##' @inheritParams gWidgets2::gbasicdialog
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gbasicdialog guiWidgetsToolkitQt
##' @S3method .gbasicdialog guiWidgetsToolkitQt
.gbasicdialog.guiWidgetsToolkitQt <- function(toolkit,
                                                 title = "Dialog",
                                                 parent=NULL,
                                                 do.buttons=TRUE,
                                                 handler = NULL,
                                                 action = NULL,
                                                 ...
                                                 ) {
 
  obj <- GBasicDialog$new(toolkit,
                          title=title, parent=parent, do.buttons=do.buttons,
                          handler=handler, action=action, 
                          ...)
  obj
}


qsetClass("BasicDialog", Qt$QDialog, function(parent=NULL, title="", do.buttons=TRUE) {
  super(parent=parent)

  setWindowTitle(title)

  this$main_lyt <- Qt$QVBoxLayout()
  this$lyt <- Qt$QVBoxLayout()

  setLayout(main_lyt)
  main_lyt$addLayout(lyt, stretch=2L)

  if(do.buttons)
    addButtons()

})

qsetMethod("addWidget", BasicDialog, function(widget) {
  lyt$addWidget(widget, stretch=2)
})

qsetMethod("addButtons", BasicDialog, function() {
 btn_box <- 
    Qt$QDialogButtonBox(Qt$QMessageBox$Cancel | 
                        Qt$QMessageBox$Ok)

 qconnect(btn_box, "accepted", function() {
   this$close()
   this$setResult(TRUE)                 # as integer
 })
 qconnect(btn_box, "rejected", 
          function() {
            this$close()
            this$setResult(FALSE)
            })

 main_lyt$addWidget(btn_box, stretch=0)
 
 
})
## class for basic dialog
GBasicDialog <- setRefClass("GBasicDialog",
                    contains="GContainer",
                    fields=list(
                      handler="ANY",
                      action="ANY"
                      ),
                    methods=list(
                      initialize=function(toolkit=NULL,
                        title = "Dialog",
                        parent=NULL,
                        do.buttons=TRUE,
                        handler = NULL,
                        action = NULL,
                        ...) {

                        if(!is.null(parent))
                          widget <<- BasicDialog(getParent(parent), title, do.buttons)
                        else
                          widget <<- BasicDialog(NULL, title, do.buttons)

                        initFields(block=widget,
                                   handler=handler,
                                   action=action)
                        
                        callSuper(toolkit)
                      },
                      add_child=function(child, ...) {
                        widget$addWidget(getBlock(child))
                        child_bookkeeping(child)
                        invisible()
                      },
                      dispose=function() {
                        widget$done(0L)
                      },
                      set_visible=function(...) {
                        widget$show()
                        widget$raise()
                        response <- widget$exec()
                        
                        h <- list(obj=.self, action=action)
                        if(response == 1) {  # 0 or 1
                          if(!is.null(handler))
                            handler(h)
                          ret <- TRUE              # was widget, but TRUE now
                        } else {
                          ret <- FALSE
                        }
                        widget$close()
                        widget$setParent(NULL)
                        return(invisible(ret))
                      }
                      ))




##' toolkit implementation of galert
##'
##' @param delay delay
##' @inheritParams gWidgets2::gaction
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .galert guiWidgetsToolkitQt
##' @S3method .galert guiWidgetsToolkitQt
.galert.guiWidgetsToolkitQt <-  function(toolkit,
                                            msg,
                                            title = "message",
                                            delay = 3,
                                            parent=NULL,
                                            ...
                                            ) {

  ## We just use a message box. Qt does not provide an InfoBar widget, like RGtk2 and there is no
  ## standard notification support
  
  if(!is.null(parent)) {
    toplevel <- getTopLevel(parent)

    parentw <- getBlock(toplevel)
    ## XXX Could use Qt$Qt$Sheet for window type here...
    mb <- Qt$QMessageBox(parentw)
  } else {
    mb <- Qt$QMessageBox()
  }
  
  
  mb$setWindowTitle(title)
  mb$setText(msg[1])
  if(length(msg) >= 2)
    mb$setInformativeText(msg[2])
   
  mb$setIcon(QtPredefinedIcons["info"])

  mb$show(); mb$raise()

  timer <- Qt$QTimer()
  timer$setSingleShot(TRUE)
  qconnect(timer, "timeout", function() mb$close())
  timer$start(as.integer(delay*1000))         # in ms
  
}

