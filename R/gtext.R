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


## We subclass to have events for this widget.
qsetClass("GQTextEdit", Qt$QTextEdit)
qsetProperty("obj", GQTextEdit)
qsetMethod("setObj", GQTextEdit, function(value) this$obj <- value)
qsetMethod("focusInEvent", GQTextEdit, function(e) {
  obj$notify_observers(signal="focusInEvent")
})
qsetMethod("focusOutEvent", GQTextEdit, function(e) {
  obj$notify_observers(signal="focusOutEvent")
})
qsetMethod("keyReleaseEvent", GQTextEdit, function(e) {

  super("keyReleaseEvent", e)
  ## run this last
  mods <- e$modifiers()                 # a flag
  modifiers <- character(0)
  if(mods & Qt$Qt$ShiftModifier) modifiers <- c(modifiers, "Shift")
  if(mods & Qt$Qt$ControlModifier) modifiers <- c(modifiers, "Ctrl")
  if(mods & Qt$Qt$MetaModifier) modifiers <- c(modifiers, "Meta")
  if(mods & Qt$Qt$AltModifier) modifiers <- c(modifiers, "Alt")

  
  obj$notify_observers(signal="keyReleaseEvent", newtext=this$text, Key=e$key(), key=e$text(), modifier=mods)
  
})

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

                         widget <<- GQTextEdit()
                         widget$setObj(.self)
                         
                         initFields(block=widget,
                                    change_signal="textChanged"
                                    )

                         
                         insert_text(text, where="beginning", font.attr=NULL, do.newline=FALSE)
                         widget$textCursor()$clearSelection()    
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
                       set_value=function(value, ..., where=NULL, do.newline) {
                         "Replace all text, pasted together with newline"
                         widget$clear()
                         value <- paste(value, collapse="\n")
                         insert_text(value, where="beginning", ..., do.newline=FALSE)
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
                           #widget$selectAll()
                           #widget$setCurrentFont(fnt)
                           widget$document()$setDefaultFont(fnt)
                           if(!is.null(value$color))
                             widget$setTextColor(Qt$QColor(value$color))
                           tc$clearSelection()
                           widget$setTextCursor(tc)
                         }
                       },
                       set_editable=function(value, ...) widget$setReadOnly(!value),
                       get_editable=function(value, ...) !widget$isReadOnly(),
                       set_word_wrap=function(value, ...) {
                         "Set word wrap mode"
                         value <- switch(value,
                                         "none"=Qt$QTextOption$NoWrap,
                                         "wrap"=Qt$QTextOption$WordWrap,
                                         "manual"=Qt$QTextOption$ManualWrap,
                                         "anywhere"=Qt$QTextOption$WrapAnywhere,
                                         QTextOption::WrapAtWordBoundaryOrAnywhere)
                         widget$setWordWrapMode(value)
                       },
                       connect_to_toolkit_signal=function(...) {}, # override
                       add_handler_changed=function(handler, action, ...) {
                         add_handler_keystroke(handler, action, ...)
                       },
                       add_handler_keystroke=function(handler, action, ...) {
                         add_handler("keyReleaseEvent", handler, action, ...)
                       },
                       add_handler_focus=function(handler, action, ...) {
                         add_handler("focusInEvent", handler, action, ...)
                       },
                       add_handler_blur=function(handler, action, ...) {
                         add_handler("focusOutEvent", handler, action, ...)
                       },
                       add_handler_selection_changed=function(handler, action=NULL, ...) {
                         signal <- "selectionChanged"
                         add_handler(signal, handler, action)
                         ## need to connect this up
                         f <- function(...) {
                           ## user.data is last value passed in
                           l <- list(...);
                           .self <- l[[length(l)]]
                           .self$notify_observers(signal=signal, ...)
                         }
                         if(is.null(connected_signals[[signal, exact=TRUE]]))
                           out <- try(qconnect(widget, signal, handler=f, user.data=.self), silent=TRUE)
                         connected_signals[[signal]] <<- TRUE
                       }
                       ))


  
