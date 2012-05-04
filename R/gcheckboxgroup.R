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
                                      out <- mapply(qinvoke, widgets, "text")
                                      out[i]
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
                                      value <- as.character(value)
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
                                    add_handler_clicked=function(handler, action=NULL, ...) {
                                      add_handler_changed(handler, action, ...)
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

                                  change_signal <<- "buttonReleased"
                                  
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

                                
                                 widget <<- Qt$QTableView()
                                 model <- Qt$QStandardItemModel(rows=0, columns=1L)
                                 widget$setModel(model)
                                 model$setParent(widget)

                                 ## no cell editing
                                 widget$editTriggers <<- Qt$QAbstractItemView$NoEditTriggers 
                                 ## alternate shading
                                 widget$setAlternatingRowColors(TRUE)
                                 ## stretch last section
                                 header <- widget$horizontalHeader()
                                 header$setStretchLastSection(TRUE)
                                 ## no visible headers
                                 widget$verticalHeader()$setVisible(FALSE)
                                 widget$horizontalHeader()$setVisible(FALSE)

                                 ## update check box when clicking item
                                 ## "clicked" is more natural, but that disables the checkbox
                                 qconnect(widget, "doubleClicked", function(index) {
                                   ## toggle
                                   model <- widget$model()
                                   item <- model$itemFromIndex(index)
                                   ## use checked enum is 0 or 2
                                   item$setCheckState(2 - as.numeric(item$checkState()))
##                                   invoke_change_handler("checkbox-changed")
                                 })
                                 qconnect(model, "itemChanged", function(item) {
                                   invoke_change_handler("checkbox-changed")
                                 })
                                 
                                 initFields(
                                            block=widget,
                                            change_signal="checkbox-changed"
                                            )

                                 set_items(items)
                                 set_index(checked)
                                
                                 add_to_parent(container, .self, ...)
                                 
                                 handler_id <<- add_handler_changed(handler, action)

                                 callSuper(toolkit)
                               },
                              connect_to_toolkit_signal=function(...) {},
                              get_value=function(drop=TRUE, ...) {
                                get_items(get_index())
                              },
                              set_value=function(value,  drop=TRUE, ...) {
                                ind <- match(value, get_items())
                                ind <- ind[!is.na(ind)]
                                set_index(ind)
                              },
                              get_index = function(...) {
                                model <- widget$model()
                                if(model$rowCount() == 0) return(integer(0))
                                
                                indices <- sapply(1:model$rowCount(), function(i) model$index(i-1,0))
                                items <- sapply(indices, function(idx) model$itemFromIndex(idx))
                                checked <- sapply(items, function(item) item$checkState() ==  Qt$Qt$Checked)
                                which(checked)
                              },
                              set_index=function(value, ...) {
                                block_observers()
                                on.exit(unblock_observers())
                                
                                model <- widget$model()
                                idx <- rep(Qt$Qt$Unchecked, model$rowCount())
                                idx[value] <- Qt$Qt$Checked
                                
                                indices <- sapply(1:model$rowCount(), function(i) model$index(i-1,0))
                                items <- sapply(indices, function(idx) model$itemFromIndex(idx))
                                mapply(qinvoke, items, "setCheckState", idx)

                                invoke_change_handler("checkbox-changed")
                              },
                              get_items = function(i, ...) {
                                model <- widget$model()
                                indices <- sapply(1:model$rowCount(), function(i) model$index(i-1,0))
                                items <- sapply(indices, function(idx) model$itemFromIndex(idx))
                                sapply(items, function(item) item$data(0))
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

                                model <- widget$model()
                                
                                ## replace or update?
                                if(missing(i)) {

                                  ## set items
                                  model$clear() # out with old
                                  m <- nrow(value)
                                  if(m == 0) # no rows
                                    return(x)
                                  
                                  for(i in 1:m) {
                                    item <- Qt$QStandardItem(value[i,1])
                                    item$setCheckable(TRUE)
                                    ## icons
                                    if(ncol(value) >=2) {
                                      icon <- getStockIconByName(value[i,2])
                                      if(!is.null(icon))
                                        item$setIcon(as_qicon(icon))
                                    }
                                    ## tooltip
                                    if(ncol(value) >= 3) {
                                      item$setToolTip(as.character(value[i,3]))
                                    }
                                    model$appendRow(item)
                                  }
                                } else {
                                  ## update items, not replace
                                  if(length(i) != nrow(value))
                                    stop(gettext("Unequal lengths for items and index"))
                                  if(nrow(value) == 0)
                                    return() # nothing to do
                                  
                                  indices <- sapply(1:model$rowCount(), function(i) model$index(i-1,0))
                                  items <- sapply(indices, function(idx) model$itemFromIndex(idx))

                                  update_item <- function(item, vals) {
                                    ## vals is list from DF extraction
                                    item$setData(vals[[1]], 0)
                                    if(length(vals) >= 2) {
                                      icon <- getStockIconByName(vals[[2]])
                                      if(!is.null(icon))
                                        item$setIcon(as_qicon(icon))
                                    }
                                    if(length(vals) >= 3)
                                      item$setToolTip(as.charactrer(vals[[3]]))
                                  }

                                  value_rows <- lapply(1:nrow(value), function(i) value[i,,drop=FALSE])
                                  mapply(update_item, items[i], value_rows)
                                  
                                }
                                  
                              },
                              get_length = function() {
                                "Number of items to choose from"
                                widget$model()$rowCount()
                              },
                              set_visible=function(value, ...) {
                                message("called set visible")
                              }
                              
                              ))
