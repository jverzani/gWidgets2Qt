##' @include GWidget.R
##' @include gqeventbox.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gimage
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gimage guiWidgetsToolkitQt
##' @S3method .gimage guiWidgetsToolkitQt
.gimage.guiWidgetsToolkitQt <-  function(toolkit,
                                         filename = "", dirname = "", stock.id=NULL, size = "",
                                         handler = NULL,action = NULL, container = NULL, ... ) {
  GImage$new( toolkit,
             filename=filename, dirname=dirname, stock.id=stock.id, size=size,
             handler = handler,action = action, container = container, ...)
}



## Main class for gimage
GImage <- setRefClass("GImage",
                      contains="GWidget",
                      fields=list(
                        "image_name"="ANY", # name (filename or stock)
                        "image_size"="ANY"  # character or width by height
                        ),
                      methods=list(
                        initialize=function(toolkit=NULL,
                          filename = "", dirname = "", stock.id=NULL, size = "",
                          handler=NULL, action=NULL, container=NULL, ...) {
                          

                          ## We use the GQEventBox to capture
                          ## mouse events. 
                          initFields(widget=Qt$QLabel(),
                                     block=GQEventBox(),
                                     change_signal="mouse-press-event"
                                     )
                          block$setObject(.self)
                          lyt <- Qt$QHBoxLayout(); block$setLayout(lyt); lyt$addWidget(widget, stretch=1L)

                          
                          image_size <<- compute_size(size)
                          ## get file or stock
                          if(!is.null(stock.id)) {
                            set_value(stock.id)

                          } else {
                            ## piece together name
                            if(nchar(dirname))
                              filename <- sprintf("%s%s%s", dirname, .Platform$file.sep, filename)
                            if(file.exists(filename)) {
                              image_name <<- filename
                            } else {
                              message(sprintf("File %s is not found", filename))
                              stop()
                            }
                            set_value(image_name)
                          }


                          add_to_parent(container, .self, ...)

                          initFields("change_signal"="mousePressEvent")
                          handler_id <<- add_handler_changed(handler, action)

                          callSuper(toolkit)
                        },
                        get_value=function( ...) {
                          image_name
                        },
                        set_value=function(value, ...) {
                          if(file.exists(value)) {
                            pix <- Qt$QPixmap()
                            pix$load(value)
                          } else {
                            ## assume stock
                            nm <- getStockIconByName(value)
                            if(is.null(nm)) {
                              message(gettext("Can't find file or stock icon for"), value)
                              return()
                            }
                            icon <- as_qicon(nm)
                            pix <- icon$pixmap(image_size[1], image_size[2])
                          }
                          widget$setPixmap(pix)                          
                          image_name <<- value
                        },
                        compute_size=function(size) {
                          "Compute size of image, default size is large"
                          def_size <- get_allocation()
                          if(def_size[1] < 1)
                            def_size <- c(200, 200) # some default if not realized
                          if(is.character(size)) {
                            size <- switch(toupper(size),
                                           "MENU"= c(16,16),
                                           "SMALL_TOOLBAR"= c(24,24),
                                           "LARGE_TOOLBAR"= c(32,32),
                                           "BUTTON"= c(20,20),
                                           "DND"= c(16,16),
                                           "DIALOG"= c(48, 48),
                                           def_size) # some crazy default
                          } else if(is.numeric(size)) {
                            size <- rep(size, length=2)
                          } else {
                            size <- def_size
                          }
                          as.integer(size)
                        },
                        ## handlers need some adjustment, as we use eventbox
                        connect_to_toolkit_signal=function(...) {}, # noop
                        handler_widget=function() {
                          block
                        },
                        add_handler_changed=function(handler, action=NULL,...) {
                          add_handler("mouse-press-event", handler, action, ...)
                        },
                        add_handler_clicked=function(...) add_handler_changed(...)
                        ))

