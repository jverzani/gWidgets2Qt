##' @include GWidget.R
NULL

##' toolkit implementation
##'
##' @inheritParams gWidgets2::gtext
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gtext guiWidgetsToolkitQt
##' @S3method .gtext guiWidgetsToolkitQt
.gtext.guiWidgetsToolkitQt <-  function(toolkit,
                    text = NULL, width = NULL, height = 300, font.attr = NULL,
                    wrap = TRUE,
                    handler = NULL, action = NULL, container = NULL,... ) {
  
  GText$new(toolkit,
            text = text, width = width, height = height,
            font.attr = font.attr, wrap = wrap,
            handler = handler, action = action, container = container, ...
            )

}


GText <- setRefClass("GText",
                     contains="GWidget",
                     fields=list(
                       buffer="ANY",
                       tag_table="ANY"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         text = NULL, width = NULL, height = 300,
                         font.attr = NULL, wrap = TRUE,
                         handler=NULL, action=NULL, container=NULL, ...) {

                         widget <<- Qt$QTextEdit()
                         initFields(block=widget,
                                    change_signal="textChanged"
                                    )

                         
                         insert_text(text, where="beginning", font.attr=NULL, do.newline=FALSE)
                         set_font(font.attr) # buffer font
                         
                         add_to_parent(container, .self, ...)
                         
                         handler_id <<- add_handler_changed(handler, action)
                         
                         callSuper(toolkit)
                       },
                       get_value=function(drop=FALSE, ...) {
                         "Return text, or selected text if drop=TRUE"
                         if(is.null(drop) || drop == FALSE) {
                           val <- widget$toPlainText()
                         } else {
                           ## return only **selected** text
                           ## if drop==TRUE
                           val <- widget$textCursor()$selectedText()
                         }
                         ## XX split by \n?
                         return(val)
                       },
                       set_value=function(value, ..., where=NULL) {
                         "Replace all text, pasted together with newline"
                         widget$clear()
                         value <- paste(value, collapse="\n")
                         insert_text(value, where="beginning", ...)
                       },
                       get_index = function(...) {
                         ## rongui request, if INDEX = TRUE return selected text
                         ## by index in the buffer
                         tc <- widget$textCursor()
                         if(tc$hasSelection()) {
                           out <- c(start=tc$selectionStart(),
                                    end=tc$selectionEnd())
                         } else {
                           out <- NA
                         }
                         return(out)
                       },
                       set_index = function(value,...) {
                         stop("Not defined")
                       },
                       get_items = function(i, j, ..., drop=TRUE) {
                         stop("Not defined")
                       },
                       set_items = function(value, i, j, ...) {
                         stop("Not defined")
                       },
                       insert_text=function(value, where, font.attr=NULL, do.newline,  ...) {
                         "Insert text into buffer. Font.attr is a vector (or list) with named quantities" 
                         if(is_empty(value))
                           return()

                         do.newline <- ifelse(missing(do.newline) || is.null(do.newline), TRUE, do.newline)

                         value <- paste(value, collapse="\n")
                         if(do.newline)
                           value <- paste(value,"\n",sep="")


                          tc <- widget$textCursor()

                         if(where == "beginning") {
                           tc$movePosition(Qt$QTextCursor$Start, Qt$QTextCursor$MoveAnchor)        # 1=start, 0=move anchor
                         } else if(where == "end") {
                           tc$movePosition(Qt$QTextCursor$End,  Qt$QTextCursor$MoveAnchor)
                         } else {
                           tc$movePosition(tc$anchor(),  Qt$QTextCursor$MoveAnchor)
                         }
                         
                         if(!is.null(font.attr)) 
                           tc$insertText(value, makeQTextCharFormat(font.attr))
                         else
                           tc$insertText(value)
                         
                         ## scroll viewport to cursor?
                         widget$setTextCursor(tc)
                         
                       },
                       set_font=function(value, ...) {

                         ## make into a list
                         value <- as.list(value)
                         fnt <- makeQFont(value)
                         
                         tc <- widget$textCursor()                   
                         if(tc$hasSelection()) {
                           widget$setCurrentFont(fnt)
                           if(!is.null(value$color))
                             widget$setTextColor(Qt$QColor(value$color))
                         } else {
                           widget$selectAll()
                           widget$setCurrentFont(fnt)
                           if(!is.null(value$color))
                             widget$setTextColor(Qt$QColor(value$color))
                           tc$clearSelection()
                           widget$setTextCursor(tc)
                         }
                       },
                       add_handler_changed=function(handler, action=NULL, ...) {
                         add_handler_keystroke(handler, action=action, ...)
                       }
                       ))


  
