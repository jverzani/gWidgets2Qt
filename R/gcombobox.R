##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gcombobox
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gcombobox guiWidgetsToolkitQt
##' @S3method .gcombobox guiWidgetsToolkitQt
.gcombobox.guiWidgetsToolkitQt <-  function(toolkit,
                                               items, selected = 1, editable = FALSE, coerce.with = NULL,
                                               handler = NULL,action = NULL, container = NULL, ... ) {

  GComboBox$new(toolkit,
                items, selected = selected, editable=editable, coerce.with = coerce.with,
                handler = handler,action = action, container = container, ...)
  
}


## We create two subclasses of this to handle editable and
## non-editable situation. These methods end up being in common for
## both.
GComboBox <- setRefClass("GComboBox",
                         contains="GWidget",
                         fields=list(
                           editable="logical"
                           ),
                         methods=list(
                           initialize=function(toolkit=NULL,
                             items,
                             selected = 1, # use 0 for blank
                             editable=FALSE,
                             coerce.with = NULL,
                             handler, action, container, ...) {
                             
                             widget <<- Qt$QComboBox()
                             if(editable) {
                               widget$setEditable(TRUE)
                               widget$setDuplicatesEnabled(FALSE)
                             }
                             
                             initFields(block=widget,
                                        coerce_with=coerce.with,
                                        change_signal=ifelse(editable, "editTextChanged", "activated"),
                                        editable=editable
                                        )
                             
                             set_items(items)
                             set_index(selected)
                             
                             add_to_parent(container, .self, ...)
                                    
                             handler_id <<- add_handler_changed(handler, action)
                                    
                             callSuper(toolkit)
                           },
                           get_index = function(...) {
                             idx <- widget$currentIndex + 1L
                           },
                           set_index = function(value,...) {
                             old_idx <- get_index()
                             idx <- min(max(-1, as.integer(value)), get_length())
                             if(idx > 0) {
                               widget$setCurrentIndex(idx - 1)
                               if(idx != old_idx)
                                 invoke_change_handler()
                             } else {
                               widget$setCurrentIndex(-1)
                             }
                           },
                           get_value=function( ...) {
                             if(editable) {
                               val <- widget$currentText
                             } else {
                               idx <- get_index()
                               if(idx == 0)
                                 return(NA)
                               val <- get_items()[idx]
                             }
                             return(val)
                           },
                           set_value=function(value, ...) {
                             if(editable) {
                               old_value <- get_value()
                               widget$setEditText(value)
                               if(old_value != value)
                                 invoke_change_handler()
                             } else {
                               ind <- pmatch(value, get_items(drop=TRUE))
                               if(!is.na(ind))
                                 set_index(ind)
                               else
                                 message("No match for ", value)
                             }
                           },
                           get_items = function(i, j, ..., drop=TRUE) {
                             items <- qdataFrame(widget$model())[,1]
                             
                             if(missing(i))
                               return(items)
                             else
                               return(items[i])
                           },
                           set_items = function(value, i, j, ...) {
                             "Set items. Indexing is ignored"
                             items <- items_to_df(value)
                             nc <- ncol(items)
                             m <- data.frame(x = items[,1], stringsAsFactors=FALSE)
                             model <- qdataFrameModel(m, useRoles=TRUE)
                             model$setParent(widget) # avoid premature garbage collection
                             
                             if(nc >= 2) {
                               icons <- items[,2]
                               qdataFrame(model)$.x.decoration <- lapply(lapply(icons, getStockIconByName), as_qicon)
                             }
                             if(nc >= 3)
                               qdataFrame(model)$.x.toolTip <- items[,3]

                             ## set if possible
                             widget$setModel(model)

#                             if(cur_idx > 0)
#                               widget$setCurrentIndex(cur_idx-1)
                             widget$update()
                           },
                           get_length = function(...) {
                             cnt <- widget$count
                             ifelse(is.null(cnt), 0, cnt)
                           },
                           ## helpers
                           items_to_df = function(items) {
                             "Return data frame from items. Data frame column names 'value', 'icon', 'tooltip'"
                             gWidgets2:::.make_gcombobox_items(items)
                           },
                           ## Handlers
                           add_handler_clicked = function(handler, action=NULL, ...) {
                             add_handler("changed", handler, action=action, ...)
                           }
                           ))

