##' @include GWidget.R
NULL

##' Toolkit gedit constructor
##'
##' @param initial.msg If this is given and \code{text} is not, then an initial message is written to prompt the user.
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gedit guiWidgetsToolkitQt
##' @S3method .gedit guiWidgetsToolkitQt
.gedit.guiWidgetsToolkitQt <-  function(toolkit,
                                           text = "", width = 25, coerce.with = NULL, initial.msg=initial.msg,
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GEdit$new( toolkit, text = text, width = width, coerce.with = coerce.with, initial.msg=initial.msg,
                    handler = handler,action = action, container = container, ...)
}

## We subclass to have events for this widget.
qsetClass("GQLineEdit", Qt$QLineEdit)
qsetProperty("obj", GQLineEdit)
qsetMethod("setObj", GQLineEdit, function(value) this$obj <- value)
qsetMethod("focusInEvent", GQLineEdit, function(e) {
  obj$notify_observers(signal="focusInEvent")
})
qsetMethod("focusOutEvent", GQLineEdit, function(e) {
  obj$notify_observers(signal="focusOutEvent")
  obj$invoke_change_handler()
})
qsetMethod("keyReleaseEvent", GQLineEdit, function(e) {

  mods <- e$modifiers()                 # a flag
  modifiers <- character(0)
  if(mods & Qt$Qt$ShiftModifier) modifiers <- c(modifiers, "Shift")
  if(mods & Qt$Qt$ControlModifier) modifiers <- c(modifiers, "Ctrl")
  if(mods & Qt$Qt$MetaModifier) modifiers <- c(modifiers, "Meta")
  if(mods & Qt$Qt$AltModifier) modifiers <- c(modifiers, "Alt")

  
  obj$notify_observers(signal="keyReleaseEvent", Key=e$key(), key=e$text(), modifier=mods)
  super("keyReleaseEvent", e)
  
})

## DND support
## This should generalize to other widgets, but for now we keep with the LineEdit widget
qsetMethod("dragEnterEvent", GQLineEdit, function(event) {
  mime_data <- event$mimeData()

  ## Our special types
  RDA_MIME_TYPE <- "application/x-rlang-transport"

  ## We need to a) be the right type and b) have a handler defined to receive drop events
  if((
      any(sapply(c(RDA_MIME_TYPE), mime_data$hasFormat)) ||
      mime_data$hasText() ||
      mime_data$hasHtml() 
      )
     &&
     length(obj$..observers[['drop-event']])
     ) {
    event$acceptProposedAction();
  } 
})
qsetMethod("dropEvent", GQLineEdit, function(event) {
  message("Drop event")
  mime_data <- event$mimeData()

  ## special types
  RDA_MIME_TYPE <- "application/x-rlang-transport"

  if(mime_data$hasHtml()) {
    ## html format
    setText(mime_data$html)
    event$acceptProposedAction()
  } else if(mime_data$hasText()) {
    ## plain text
    txt <- mime_data$text()
    if(length(obj$..observers[['drop-event']])) {
      try(obj$notify_observers(signal="drop-event", dropdata=txt), silent=TRUE)
    }
    event$acceptProposedAction()
  } else if(mime_data$hasFormat(RDA_MIME_TYPE)) {
    name_list <- unserialize(mime_data$data(RDA_MIME_TYPE))
    if (length(name_list) && is.character(name_list[[1]])) {
      txt <- name_list[[1]]
      if(length(obj$..observers[['drop-event']])) {
        try(obj$notify_observers(signal="drop-event", dropdata=txt), silent=TRUE)
      }
    }
    event$acceptProposedAction()
  } else {
    super("dropEvent", event)
  }
})



## Validator framework
## Set the validator function through the \code{set_validator} method of the object
## This is a function returning a Boolean with its input being the string
qsetClass("GQEditValidator", Qt$QValidator)
qsetProperty("Fun", GQEditValidator)
qsetMethod("setFun", GQEditValidator, function(FUN) this$Fun <- FUN)
qsetMethod("validate", GQEditValidator, function(input, pos) {
  if(is.null(this$Fun)) {
    Qt$QValidator$Acceptable
  } else if(nchar(input) == 0) {
    Qt$QValidator$Intermediate
  } else {
    ifelse(this$Fun(input),
           Qt$QValidator$Acceptable,
           Qt$QValidator$Invalid)
  }
})

## The \code{GEdit} class can be a drop target or source. To be a drop target, one must call addDropTarget, as no
## default handler is set. The value h$dropdata is used to parameterize the call.
GEdit <- setRefClass("GEdit",
                            contains="GWidget",
                            fields=list(
                              completer="ANY",
                              model="ANY",
                              validator="ANY",
                              coerce_with="ANY"
                              ),
                            methods=list(
                              initialize=function( toolkit=NULL,
                                text = "", width = 25, coerce.with = NULL,
                                initial.msg="",
                                handler = NULL, action = NULL, container = NULL, ..., fill=NULL) {

                                widget <<- GQLineEdit()
                                widget$setObj(.self)
                                
                                initFields(block=widget,
                                           coerce_with=coerce.with,
                                           completer=NULL,
                                           validator=NULL,
                                           change_signal="editingFinished"
                                           )

                                ## completion framework
                                completer <<- Qt$QCompleter()
                                model <<- Qt$QStandardItemModel()

                                widget$setCompleter(completer)
                                completer$setModel(model)
                                qconnect(completer, "activated(QString)", function(txt) {
#                                  block_observers()
#                                  on.exit(unblock_observers())
                                  set_value(txt)
                                })

                                ## configure for drop and drop
                                widget$setAcceptDrops(TRUE)
                                widget$setDragEnabled(TRUE)
                                
                                ## set up text
                                if(nchar(initial.msg) > 0)
                                  set_init_txt(initial.msg)

                                if(nchar(text) > 0)
                                  set_value(text)

                                ## fill hack
                                if(is(container, "GBoxContainer") && (missing(fill) || is.null(fill)))
                                  fill <- "x"
                                
                                add_to_parent(container, .self, ..., fill=fill)
                                
                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              set_value=function(value,drop=TRUE, ...) {
                                widget$setText(value)
                                invoke_change_handler()
                              },
                              get_value=function(drop=TRUE, ...) {
                                  widget$text
                              },
                              get_items=function(i, j, ..., drop=TRUE) {
                                "i for index"
                                n <- model$rowCount()
                                if(n == 0)
                                  return(character(0))
                                else
                                  vals <- lapply(seq_len(n), function(i) {
                                    item <- mod$item(i-1)
                                    item$text()
                                  })

                                vals[i]
                              },
                              set_items=function(value, i, j, ...) {
                                if(missing(i)) {
                                  vals <- value
                                } else {
                                  vals <- get_items()
                                  vals[i] <- value
                                }
                                
                                ## clear model??
                                n <- length(vals)
                                lapply(seq_len(n), function(i) {
                                  item <- Qt$QStandardItem(vals[i])
                                  model$setItem(i - 1, item)
                                })
                              },
                              
                              get_visible = function() {
                                widget$echoMode() == Qt$QLineEdit$Normal
                              },
                              set_visible = function(value) {
                                "Show password characters?"
                                widget$setEchoMode(ifelse(visible, Qt$QLineEdit$Normal, Qt$QLineEdit$Password))
                              },

                              get_editable=function() {
                                "Can we actually edit widget?"
                                widget$getEditable()
                              },
                              get_editable=function(...) !widget$isReadOnly(),
                              set_editable=function(value, ...) widget$setReadOnly(!value),
                              can_undo=function() widget$undoAvailable,
                              undo = function() widget$undo(),
                              can_redo=function() widget$redoAvailable,
                              redo = function() widget$redo(),
                              set_init_txt=function(txt) {
                                "set initial text, gray out"
                                widget$setPlaceholderText(txt)
                              },

                              ## Handlers
                              connect_to_toolkit_signal=function(...) {},
                              add_handler_keystroke=function(handler, action=NULL, ...) {
                                add_handler("keyReleaseEvent", handler, action, ...)
                              },
                              add_handler_focus=function(handler, action, ...) {
                                add_handler("focusInEvent", handler, action, ...)
                              },
                              add_handler_blur=function(handler, action, ...) {
                                add_handler("focusOutEvent", handler, action, ...)
                              },
                              
                              ## Handler: changed -> clicked
                              ## add_handler_changed = function(handler, action=NULL, ...) {
                              ##   if(missing(handler) || is.null(handler))
                              ##     return()
                              ##   f <- function(h, widget, event, ...) {
                              ##     keyval <- event$GetKeyval()
                              ##     if(keyval == GDK_Return) {
                              ##       handler(h, widget, event, ...)
                              ##       return(TRUE)
                              ##     } else {
                              ##       return(FALSE)
                              ##     }
                              ##   }
                              ##   add_handler("activate", f, action=action, ...)
                              ## },
                             

                              ## Validation methods
                              ## This is Qt Specific
                              set_validator = function(FUN) {
                                "Set a function to do the validation. Use FUN=NULL to remove. FUN is a function of single variable and returns a logical"
                                validator <<- GQEditValidator()
                                validator$setFun(FUN)
                                widget$setValidator(validator)
                              },
                              ## Basic validation

## background-image: url(:/icons/icons/table_add_16.png);
## background-repeat: no-repeat;
## background-position: center right;
## padding-right: 17px;
## height: 27px

                              invalid_style=function() {
                                "Style for invalid entry. Cf. http://doc.qt.nokia.com/4.7-snapshot/stylesheet-examples.html"
                                out <- "
  border: 2px solid red;
  border-radius: 10px;
  padding: 0 8px;
  selection-background-color: darkgray;
";
                                return(out)
                              },
                              set_invalid=function(value, msg=NULL) {
                                ## adjust widget
                                if(value) {
                                  widget$setStyleSheet(invalid_style())
                                  widget$setToolTip(msg)
                                } else {
                                  widget$setStyleSheet("")
                                  widget$setToolTip("")                                  
                                }
                                
                                ## next
                                callSuper(value, msg)
                              }
                              ))

