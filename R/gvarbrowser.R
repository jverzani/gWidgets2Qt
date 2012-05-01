##' @include gtree.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gvarbrowser
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gvarbrowser guiWidgetsToolkitQt
##' @S3method .gvarbrowser guiWidgetsToolkitQt
.gvarbrowser.guiWidgetsToolkitQt <-  function(toolkit,
                                                 handler = NULL,action = "summary", container = NULL, ... ) {
  GVarBrowser$new(toolkit,
                  handler = handler,action = action, container = container, ...)
}

## TODO:
## =====
## * add in popup menu with common actions: rm, ...


qsetClass("GQStandardItemModel", Qt$QStandardItemModel)
qsetProperty("obj", GQStandardItemModel)

qsetMethod("mimeData", GQStandardItemModel, function(lst) {
  if(length(lst) == 0)
    super("mimeData", lst)

  idx <- lst[[1]]
  path <- obj$path_from_index(idx)$path[-1]
  if(length(path) == 0)
    super("mimeData", lst)

  data <- Qt$QMimeData()
  txt <- obj$notify_observers(signal="drag-event", drag_data=path)[[1]]
  data$setText(txt)
  
  data
})


## Class for variable browser.
GVarBrowser <- setRefClass("GVarBrowser",
                            contains="GTreeBase",
                          fields=list(
                             "ws_model"="ANY",
                             "filter_classes"="list",
                            "filter_name"="character",
                            "other_label"="character",
                            "timer"= "ANY",
                            "use_timer"="logical",
                            "item_list"="list"
                             ),
                            methods=list(
                              initialize=function(toolkit=NULL,
                                handler=NULL, action=NULL, container=NULL, ..., fill=NULL) {

                                ws_model <<- gWidgets2:::WSWatcherModel$new()
                                o = gWidgets2:::Observer$new(function(self) {self$update_view()}, obj=.self)
                                ws_model$add_observer(o)

                                widget <<-  Qt$QTreeView()
                                model <- GQStandardItemModel(rows=0, columns=2) # name, summary
                                model$obj <- .self
                                
                                model$setHorizontalHeaderLabels(c(gettext("Variable"), gettext("Summary")))
                                widget$setAlternatingRowColors(TRUE)
                                widget$setIndentation(14) # tighten up
                                
                                ## But how to recover the dragged object?
                                ## it is in raw format:
                                ## mime_data$data("application/x-qabstractitemmodeldatalist")
                                
                                
                                widget$setModel(model)
                                model$setParent(widget) # avoid early gc
                                
                                widget$setEditTriggers(Qt$QAbstractItemView$NoEditTriggers)
                                widget$setSelectionBehavior(Qt$QAbstractItemView$SelectRows)
                                widget$setSelectionMode(Qt$QAbstractItemView$MultiSelection) # multiple selection

                                initFields(block=widget,
                                           change_signal="activated",
                                           item_list=list(),
                                           filter_classes=gWidgets2:::gvarbrowser_default_classes,
                                           filter_name="",
                                           other_label="Other",
                                           use_timer=TRUE
                                           )
                                
                                ## set up drag source
                                add_drop_source(function(h,...) {
                                  svalue(h$obj)
                                })
                                
                                add_context_menu()


                                ## fill hack
                                if(is(container, "GBoxContainer") && (missing(fill) || is.null(fill)))
                                  fill <- "both"
                                
                                add_to_parent(container, .self, ..., fill=fill)

                                handler_id <<- add_handler_changed(handler, action)

                                ## Try our own timer. 
                                timer <<- gtimer(1000, function(...) .self$ws_model$update_state())
                                
                                populate_view()

                                
                                callSuper(toolkit)
                              },
                              start_timer=function() {
                                if(use_timer)
                                  timer$start_timer()
                              },
                              stop_timer=function() timer$stop_timer(),
                              adjust_timer=function(ms) {
                                "Adjust interval to size of workspace"
                                if(missing(ms)) {
                                  n <- length(ls(envir=.GlobalEnv))
                                  ms <- 1000 * floor(log(5 + n, 5))
                                }
                                timer$set_interval(ms)
                              },
                              set_filter_name=function(value) {
                                message("Setting a regular expression to filter the displayed objects is not supported")
                                return()
                                filter_name <<- value
                                populate_view()
                              },
                              set_filter_classes=function(value) {
                                filter_classes <<- value
                                populate_view()
                              },
                              ##
                              add_value=function(x, name, parent_item, item=NULL) {
                                "Add a row to the model"
                                if(is.null(item))
                                  item <- Qt$QStandardItem(name)
                                else
                                  item$setData(name)
                                
                                summary_item <- Qt$QStandardItem(gWidgets2:::short_summary(x))
                                icon <- getStockIconByName(stockIconFromObject(x))
                                if(!is.null(icon))
                                  item$setIcon(as_qicon(icon))
                                ## tooltip?
                                
                                if(is.null(item$parent())) {
                                  parent_item$appendRow(list(item, summary_item))
                                }

                                ## store in lookup if appropriate
                                if(is.null(parent_item$parent())) {
                                  item_list[[name]] <<- item
                                } 
                             
                                
                                ## recurse if needed
                                if(is.list(x) && !is.null(attr(x, "names"))) {
                                  item$setRowCount(0L) # clear out if there
                                  nms <- names(x)
                                  sapply(seq_along(x), function(i) add_value(x[[i]], nms[i], item))
                                }
                                ## return item
                                invisible(item)
                              },
                              clear_items=function() {
                                "Clear old items"
                                model <- widget$model()
                                cnt <- model$rowCount()
                                sapply(rev(seq_len(cnt)) - 1, function(i) {
                                  root <- model$indexFromItem(model$invisibleRootItem())
                                  model$removeRow(i, root)
                                })
                              },
                              populate_view=function(...) {
                                "Initialize tree. Afterwards we only modify values"
                                ## we need to update top-level object
                                ## use filter_classes to break up object
                                clear_items()
                                root <- widget$model()$invisibleRootItem()


                                  ## do categories
                                  categories <- names(filter_classes) # also "Other"
                                  category_color <- Qt$QBrush(qcolor(0, 0, 255))
                                  

                                  
                                  for(i in categories) {
                                    item <- Qt$QStandardItem(i)
                                    item$setForeground(category_color)
                                    root$appendRow(item)
                                    ## what to add
                                    klasses <- filter_classes[[i]]
                                    out <- ws_model$get_by_function(function(y)  length(Filter(function(x) is(y, x), klasses) > 0))
                                    out_names <- names(out)
                                    idx <- order(out_names)

                                    if(length(out))
                                      sapply(seq_along(out), function(i) add_value(out[idx][[i]], out_names[idx][i], item))
                                  }
                                  ## other
                                  item <-  Qt$QStandardItem(gettext(other_label))
                                  item$setForeground(category_color)
                                  root$appendRow(item)
                                  
                                  klasses <- unlist(filter_classes)
                                  out <- ws_model$get_by_function(function(y)  !(length(Filter(function(x) is(y, x), klasses) > 0)))
                                  out_names <- names(out)
                                  idx <- order(out_names)
                                  out <- out[idx]; out_names <- out_names[idx]
                                  if(length(out))
                                    sapply(seq_along(out), function(i) add_value(out[[i]], out_names[i], item))


                                start_timer()
                                
                              },
                              update_view=function(...) {
                                "Update view of objects"
                                stop_timer()
                                on.exit({adjust_timer(); start_timer()})

                                ## for items in the global workspace.
                                remove_item <- function(nm) {
                                  item <-  item_list[[nm, exact=TRUE]]
                                  item_list[[nm]] <<- NULL
                                  item$parent()$removeRow(item$row())
                                }
                                add_item <- function(x, nm) {
                                  type <- Filter(function(i) any(sapply(i, "is", object=x)), filter_classes)
                                  if(length(type) == 0)
                                    type <- gettext("Other") # catch all
                                  else
                                    type <- names(type)
                                  ## add to type, then sort within ... too much sorting
                                  parent_item <- widget$model()$findItems(type)[[1]]
                                  item <- add_value(x, nm, parent_item)
                                  parent_item$sortChildren(0L)
                                  ## cache
                                  item_list[[nm]] <<- item
                                }
                                update_item <- function(x, nm) {
                                  remove_item(nm)
                                  add_item(x, nm)
                                }

                                
                                ## We use item_list as a cache to do most of the work
                                if(nchar(filter_name)) {
                                  objs <- ws_model$filter_names(function(x) {
                                    force(filter_name)
                                    grepl(filter_name, x)
                                  })
                                  
                                } else {
                                  ## use changes
                                  changes <- ws_model$changes
                                  
                                  mapply(remove_item, changes$removed) # name only, object is gone
                                  mapply(add_item, mget(changes$added, .GlobalEnv), changes$added)


                                  mapply(update_item, mget(changes$changed, .GlobalEnv), changes$changed)
                                }
                              },
                              ##
                              get_value=function(drop=TRUE, ...) {
                                "Get selected values as names or objects if drop=FALSE"
                                out <- callSuper("get_value", drop=FALSE)
                                if(!is.list(out)) ## work with lists
                                  out <- list(out)
                                if(nchar(filter_name) == 0) 
                                    out <- lapply(out, "[", -1)
                                
                                if(length(out) == 0)
                                  return(character(0)) 

                                  nms <- lapply(out, function(x) {
                                    sapply(x, function(i) ifelse(grepl("\\s", i),
                                                                 sprintf("'%s'", i),
                                                                 i))
                                  })
                                nms <- lapply(nms, paste, collapse="$")
                                
                                if(is.null(drop) || drop) {
                                  ## return non "" names
                                  Filter(nchar, nms)
                                } else {
                                  ## return objects, not values
                                  out <- lapply(out, gWidgets2:::get_object_from_string)
                                  names(out) <- nms
                                  ind <- which(nms == "")
                                  out <- out[ind]
                                  if(length(out) == 1)
                                    out[[1]]
                                  else
                                    out
                                }
                              },

                              set_value=function(value, ...) {
                                "Select and open value given."
                              },
                              ## context menu popup
                              add_context_menu=function() {
                                return()
                                ## XXX update
                                ## make context sensitive menu. Requires identifying value of selected
                               
                              },
                              ## selection is changed
                              add_handler_selection_changed=function(handler, action=NULL, ...) {
                                add_handler("selectionChanged", handler, action, emitter=widget$selectionModel())
                              }
                              ))
