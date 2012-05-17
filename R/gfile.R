##' @include GWidget.R
NULL

##' Toolkit implementation
##'
##' @inheritParams gWidgets2::gfile
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gfile guiWidgetsToolkitQt
##' @S3method .gfile guiWidgetsToolkitQt
.gfile.guiWidgetsToolkitQt <- function(toolkit,
                                          text = "",
                                          type = c("open","save","selectdir"),
                                          initial.filename = NULL,
                                          filter =  list(
                                            "All files"=list(
                                              patterns=c("*")
                                              ),
                                            "R files"=list(
                                              patterns=c("*.R","*.Rdata")
                                              ),
                                            "text files"=list(
                                              mime.types=c("text/plain")
                                              )
                                            ),
                                          multi=FALSE,
                                          ...) {
  ## make dialog, return character class object (character(0) if no selectino)


  fm <- Qt$QFileDialog()

  ## different things depending on type
  type <- match.arg(type)
  if(type == "open") {
    
    filters <- c()
    if(!is.null(filter)) {
      for(i in names(filter)) {
        if(!is.null(filters[[i]]$pattern)) {
          filters <- c(filters, paste(i, " (", paste(filters[[i]]$patterns, collapse=" "),
                                      ")", sep=""))
        }
        ## no mime.types
      }
      out <- sapply(filters, function(i) is.null(i$mime.types))
      if(any(out))
                  XXX("No filtering of mime types, only patterns")
    }
    
    if(length(filters) == 0)
      filters <- c("All files (*.*)")
    
    theFilter <- paste(filters, collapse=";;")
    
    ## how to set Title
    fm$setNameFilter(theFilter)
    fm$setDirectory(getwd())
    if(!is.null(initial.filename))
      fm$selectFile(basename(initial.filename))
    
    if(multi)
      fm$setFileMode(Qt$QFileDialog$ExistingFiles)
  } else if(type == "save") {
    ## Save
    if(!is.null(initial.filename))
      fm$selectFile(basename(initial.filename))
    fm$setConfirmOverwrite(TRUE)
    fm$setFileMode(Qt$QFileDialog$AnyFile)
    
  } else if(type == "selectdir") {
    
    fm$setConfirmOverwrite(TRUE)
    fm$setFileMode(Qt$QFileDialog$Directory)
    fm$setOption(Qt$QFileDialog$ShowDirsOnly, TRUE)   # directory only
  }

  ret <- fm$exec()

  if(ret == 1) {
    val <- fm$selectedFiles()
    return(val)
  } else {
    ## cancel
    return(character(0))
  }
             
}
                                          

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gfilebrowse guiWidgetsToolkitQt
##' @S3method .gfilebrowse guiWidgetsToolkitQt
.gfilebrowse.guiWidgetsToolkitQt <-  function(toolkit,
                                                 text = "",
                                                 type = c("open","save","selectdir"),
                                                 initial.filename = NULL,
                                                 filter = list(),
                                                 quote=TRUE,
                                                 handler=NULL,
                                                 action=NULL,
                                                 container = NULL,
                                                 ... ) {
  GFileBrowse$new(toolkit,
            text=text, type=type, initial.filename=initial.filename,
            filter=filter, quote=quote, handler=handler, action=action, container=container, ...)
}


## XXX
GFileBrowse <- setRefClass("GFileBrowse",
                           contains="GWidget",
                           methods=list(
                              initialize=function(
                                toolkit=NULL,
                                text = "",
                                type = c("open", "save", "selectdir"),
                                initial.filename = NULL,
                                filter = list(),
                                quote=TRUE,
                                handler=NULL,
                                action=NULL,
                                container = NULL,
                                ... ) {
                                
                                widget <<- Qt$QLineEdit()
                                block <<- Qt$QWidget()

                                initFields(widget = Qt$QLineEdit(),
                                           block = Qt$QWidget(),
                                           change_signal="returnPressed")

                                lyt <- Qt$QHBoxLayout()
                                block$setLayout(lyt)
                                

                                
                                btn <- Qt$QPushButton("file")
                                btn$setIcon(Qt$QApplication$style()$standardIcon(Qt$QStyle$SP_FileIcon))

                                lyt$addWidget(widget, stretch=2L)
                                lyt$addWidget(btn, stretch=0L)

                                qconnect(btn, "clicked", function() {
                                  ## quick dispatch by calling within toolkit
                                  ret <- .gfile.guiWidgetsToolkitQt(toolkit=toolkit, text=text, type=type,
                                                                    initial.filename=initial.filename,filter=filter)

                                  if(length(ret))
                                    set_value(ret[1])
                                  
                                })
                                
                                handler_id <<- add_handler_changed(handler, action)


                                add_to_parent(container, .self, ...)
                                callSuper(toolkit)
                              },
                              get_value=function( ...) {
                                x <- widget$text
                                Encoding(x) <- "UTF-8"
                                x
                              },
                              set_value=function(value, ...) {
                                ## should we check file.exists?
                                if(file.exists(value)) {
                                  widget$setText(value)
                                  invisible(notify_observers(signal=change_signal))
                                }
                              }
                              ))

