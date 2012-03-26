##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gcheckboxgroup
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gcheckboxgroup guiWidgetsToolkitQt
##' @S3method .gcheckboxgroup guiWidgetsToolkitQt
.gcheckboxgroup.guiWidgetsToolkitQt <-  function(toolkit=NULL,
                                                    items, checked = FALSE, horizontal = FALSE,
                                                    use.table=FALSE, handler = NULL,
                                                    action = NULL, container = NULL, ... ) {
  if(use.table)
    GCheckboxGroupTable$new(toolkit, items, checked = checked,
                            handler = handler,action = action,  container = container, ...)
  else
    GCheckboxGroup$new(toolkit,
                       items, checked = checked, horizontal = horizontal,
                       handler = handler, action = action, container = container, ...)
}

## a widget group buttons
GButtonGroupWidget <- setRefClass("GButtonGroupWidget",
                                  contains="GWidget",
                                  fields=list(
                                    button_group="ANY",
                                    constructor="ANY"
                                    ),
                                  methods=list(
                                    get_value=function(drop=TRUE, ...) {
                                      items <- get_items()
                                      items[get_index()]
                                    },
                                    set_value=function(value, drop=TRUE, ...) {
                                      if(is.logical(value)) {
                                        set_index(value)
                                      } else {
                                        items <- get_items()
                                        ind <- items %in% value
                                        set_index(ind)
                                      }
                                    },
                                    get_items = function(i, ...) {
                                      widgets <- button_group$buttons()
                                      mapply(qinvoke, widgets, "text")
                                    },
                                    remove_old_items=function() {
                                      "Clear out old, we are replacing"
                                      sapply(button_group$buttons(), function(i) {
                                        button_group$removeButton(i)
                                        widget$removeWidget(i)
                                        i$setParent(NULL)
                                      })
                                    },
                                    set_items = function(value, i, ...) {
                                      if(missing(i)) {
                                        remove_old_items()
                                        ## make widgets, add to button group, layout
                                        widgets <- sapply(value, constructor)
                                        sapply(widgets, button_group$addButton)
                                        sapply(widgets, function(i) widget$addWidget(i))
                                      } else {
                                        widgets <- button_group$buttons()
                                        mapply(qinvoke, widgets[i], "setText", value)
                                      }
                                      invisible()
                                    },
                                    get_names=function(...) 
                                    mapply(qinvoke, button_group$buttons(), "text"),
                                    set_names=function(value, ...) mapply(qinvoke, button_group$buttons(), "setText", value),
                                    get_length = function() {
                                      base:::length(button_group$buttons())
                                    },
                                    ## Handler: changed -> clicked
                                    handler_widget=function() button_group,
                                    add_handler_changed=function(handler, action=NULL, ...) {
                                      add_handler_clicked(handler, action, ...)
                                    },
                                    add_handler_clicked=function(handler, action=NULL, ...) {
                                      add_handler("buttonReleased", handler, action, ...)
                                    }
                                    ))


## checkbox group class
GCheckboxGroup <- setRefClass("GCheckboxGroup",
                              contains="GButtonGroupWidget",
                              methods=list(
                                initialize=function(toolkit,
                                  items, checked, horizontal = FALSE,
                                  handler = NULL,
                                  action = NULL, container = NULL, ...
                                  ) {
                                  
                                  if(horizontal)
                                    widget <<-  Qt$QHBoxLayout()
                                  else
                                    widget <<- Qt$QVBoxLayout()
                                  block <<- Qt$QWidget()
                                  block$setLayout(widget)
                                  
                                  constructor <<- Qt$QCheckBox
                                  
                                  button_group <<- Qt$QButtonGroup()
                                  button_group$setExclusive(FALSE)
                                  
                                  set_items(value=items)
                                  set_index(checked)
                                  add_to_parent(container, .self, ...)
                                  
                                  handler_id <<- add_handler_changed(handler, action)
                                  
                                  callSuper(toolkit)
                                },
                                get_index = function(...) {
                                  "Return indices, not logical"
                                  widgets <- button_group$buttons()
                                  which(sapply(widgets, function(i) i$checkState() == 2))
                                },
                                set_index=function(value, ...) {
                                  block_observer()
                                  if(is.numeric(value)) 
                                    value <- seq_len(get_length()) %in% value
                                  value <- rep(value, length=get_length())
                                  widgets <- button_group$buttons()
                                  mapply(qinvoke, widgets, "setChecked", value)
                                  unblock_observer()
                                  notify_observers(signal="toggled") ## XXX
                                  invisible()
                                }
                                
                                ))
                                
                                
## uses table for checkboxes
GCheckboxGroupTable <-  setRefClass("GCheckboxGroupTable",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit,
                                items, checked = FALSE,
                                handler = NULL,
                                action = NULL, container = NULL, ... ) {

                                 widget <<- Qt$QTableWidget()
                                 widget$setColumnCount(1)
                                 ## alternate shading
                                 widget$setAlternatingRowColors(TRUE)
                                 ## stretch last section
                                 header <- widget$horizontalHeader()
                                 header$setStretchLastSection(TRUE)
                                 ## no visible headers
                                 widget$verticalHeader()$setVisible(FALSE)
                                 widget$horizontalHeader()$setVisible(FALSE)

                                 initFields(
                                            block=widget,
                                            change_signal="itemChanged"
                                            )

                                 set_items(items)
                                 set_index(checked)
                                
                                 add_to_parent(container, .self, ...)
                                 
                                 handler_id <<- add_handler_changed(handler, action)

                                 callSuper(toolkit)
                               },
                              get_value=function(drop=TRUE, ...) {
                                get_items(get_index())
                              },
                              set_value=function(value,  drop=TRUE, ...) {
                                ind <- match(value, get_items())
                                ind <- ind[!is.na(ind)]
                                set_index(ind)
                              },
                              get_index = function(...) {
                                n <- widget$rowCount
                                if(n == 0)
                                  return(logical(0))
            
                                vals <- sapply(1:n, function(i) {
                                  item <- widget$item(i-1, 0)
                                  as.logical(item$checkState()) ## 0 -> FALSE, 2 -> TRUE
                                })
                                which(vals)
                              },
                              set_index=function(value, ...) {
                                n <- get_length()
                                if(n < 1) return()
                                if(is.logical(value)) {
                                  ## recycle
                                  value <- as.logical(rep(value, length=n))
                                } else {
                                  ## integer, convert
                                  value <- 1:n %in% value
                                }
                                state <- sapply(value, function(i) ifelse(i, Qt$Qt$Checked, Qt$Qt$Unchecked))
                                lapply(1:n, function(i) {
                                  item <- widget$item(i-1, 0)
                                  item$setCheckState(state[i])
                                })
                              },
                              get_items = function(i, ...) {
                                n <- get_length()
                                if(n == 0) return(character(0))
                                
                                items <- sapply(seq_len(n), function(i) {
                                  item <- widget$item(i-1, 0)
                                  item$text()
                                })
                                items[i]
                              },
                              set_items = function(value, i, ...) {

                                block_observers()
                                on.exit(unblock_observers())
                                
                                ## value can be a vector or data frame
                                ## if a data.frame we have
                                ## text, [stockicon, [tooltip]]
                                if(is.matrix(value))
                                  value <- data.frame(value, stringsAsFactors=FALSE)
                                else if(!is.data.frame(value))
                                  value <- data.frame(value, stringsAsFactors=FALSE)
                                
                                            
                                ## get i
                                if(missing(i)) {
                                  widget$setRowCount(nrow(value))
                                  i <- seq_len(nrow(value))
                                }
            
                                if(is.logical(i))
                                  i <- which(i)

                                ## set items
                                widget$clear()
                                m <- nrow(value)
                                if(m == 0)
                                  return(x)
            
                                lapply(1:m, function(i) {
                                  item <- Qt$QTableWidgetItem(as.character(value[i,1]))
                                  flags <- Qt$Qt$ItemIsEditable | Qt$Qt$ItemIsUserCheckable | Qt$Qt$ItemIsEnabled
                                  item$setFlags(flags) 
                                  
                                  item$setCheckState(Qt$Qt$Unchecked) # default, adjust

                                  if(ncol(value) >= 2) {
                                    icon <- value[i,2]
                                    icon <- getStockIconFromName(icon)
                                    if(!is.null(icon)) 
                                      item$setIcon(icon)
                                  }
                                  
                                  if(ncol(value) >= 3) {
                                    tooltip <- value[i,3]
                                    if(!is.null(tooltip))
                                      item$setToolTip(tooltip)
                                  }
                                  
                                  widget$setItem(i - 1, 0, item)
                                })            
                              },
                              get_length = function() {
                                "Number of items to choose from"
                                widget$rowCount
                              }

                              ))
