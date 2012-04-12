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
qsetMethod("keyPressEvent", GQLineEdit, function(e) {

  mods <- e$modifiers()                 # a flag
  modifiers <- character(0)
  if(mods & Qt$Qt$ShiftModifier) modifiers <- c(modifiers, "Shift")
  if(mods & Qt$Qt$ControlModifier) modifiers <- c(modifiers, "Ctrl")
  if(mods & Qt$Qt$MetaModifier) modifiers <- c(modifiers, "Meta")
  if(mods & Qt$Qt$AltModifier) modifiers <- c(modifiers, "Alt")

  
  obj$notify_observers(signal="keyPressEvent", Key=e$key(), key=e$text(), modifier=mods)
  super("keyPressEvent", e)
  
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

##' The GEdit class adds some methods beyond the spec: \code{set_error}, \code{clear_error}, \code{validate_value}
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
                                handler = NULL, action = NULL, container = NULL, ...) {

                                widget <<- GQLineEdit()
                                widget$setObj(.self)
                                
                                initFields(block=widget,
                                           coerce_with=coerce.with,
                                           completer=NULL,
                                           validator=NULL,
                                           change_signal="returnPressed"
                                           )

                                ## completion framework
                                completer <<- Qt$QCompleter()
                                model <<- Qt$QStandardItemModel()

                                widget$setCompleter(completer)
                                completer$setModel(model)
                                qconnect(completer, "activated(QString)", function(txt) {
                                  block_observers()
                                  set_value(txt)
                                  unblock_observers()
                                })

                                
                                ## set up text
                                if(nchar(initial.msg) > 0)
                                  set_init_txt(initial.msg)

                                if(nchar(text) > 0)
                                  set_value(text)

                                add_to_parent(container, .self, ...)
                                
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
                              set_editable = function(value, j) {
                                widget$setEditable(as.logical(value))
                              },
                              set_init_txt=function(txt) {
                                "set initial text, gray out"
                                widget$setPlaceholderText(txt)
                              },

                              ## Handlers
                              add_handler_keystroke=function(handler, action=NULL, ...) {
                                add_handler("keyPressEvent", handler, action, ...)
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
                             

                              ## Extra methods
                              set_validator = function(FUN) {
                                "Set a function to do the validation. Use FUN=NULL to remove"
                                validator <<- GQEditValidator()
                                validator$setFun(FUN)
                                widget$setValidator(validator)
                              },
                              set_invalid=function(value, msg=NULL) {
                                if(value) {
                                  ## XXX work on this. 
                                  ## an error
                                  if(!is.null(msg))
                                    warning(msg)
                                } else {
                                  ## clear error

                                }

                              }
                              ))

