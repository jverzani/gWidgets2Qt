##' @include GWidget.R
##' @include gmenu.R
##' @include dialogs.R
##' @include gtable.R
NULL

## TODO
## * header handlers
## * column drag and drop
## * size override for passing in column sizes through a list.
## * popup menu?


##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gdf
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gdf guiWidgetsToolkitQt
##' @S3method .gdf guiWidgetsToolkitQt
.gdf.guiWidgetsToolkitQt <-  function(toolkit,
                                         items = NULL,
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GDf$new(toolkit,
           items=items, 
           handler = handler, action = action, container = container, ...)
}


qsetClass("GQTableView", Qt$QTableView)


GDf <- setRefClass("GDf",
                   contains="GWidget",
                   fields=list(
                     qmodel="ANY"
                     ),

                   methods=list(
                     initialize=function(toolkit, items, name=deparse(substitute(df)),
                       handler=NULL, action=NULL,
                       container=NULL,
                       ..., fill=NULL) {

                       if(missing(items))
                         items <- data.frame(missing=NA)
                       items <- as.data.frame(items)

                       ## the qdataFrameModel makes this *much* easier to write
                       qmodel <<- qdataFrameModel(items, editable=names(items))
                       widget <<- Qt$QTableView()
                       widget$setModel(qmodel)
                       qmodel$setParent(widget)

                       ## Michaels text formatting delagate does all the hard work:
                       delegate <- qrTextFormattingDelegate(view)
                       widget$setItemDelegate(delegate)

                       
                       initFields(block=widget)

                       ## menus only good once realized
                       ## add popup?
                       if(is(container, "GBoxContainer") && (missing(fill) || is.null(fill)))
                         fill <- "both"
                       
                       add_to_parent(container, .self, ..., fill=fill)
                       
                       ##handler_id <<- add_handler_changed(handler, action)
                       
                       callSuper(toolkit)
                     },
                     ## methods
                     get_value=function(drop=TRUE, ...) {
                       "Geet selected values"
                       l <- get_selected()
                       if(length(l[[1]]) == 0)
                         return(NULL)  # ?? what is right?
                         
                       DF <- qdataFrame(qmodel)
                       with(l, DF[rows, columns, drop=drop])
                     },
                     set_value=function(...) {
                       "No method to set by value, try set_index"
                     },
                     get_index=function(...) {
                       get_selected(...)
                     },
                     set_index=function(value, ...) {
                       "Set index of selected. Index is list(rows=rows, columns=columns)"
                       set_selected(value[[1]], value[[2]])
                     },
                     get_items = function(i, j, ..., drop=TRUE) {
                       qdataFrame(qmodel)[i, j, drop=drop]
                     },
                     set_items = function(value, i, j, ...) {
                       if(missing(i) && missing(j)) {
                         ## replace the whole shebang
                         qmodel <<- qdataFrameModel(value)
                         widget$setModel(qmodel)
                         qmodel$setParent(widget)
                       } else {
                         ## else modify
                         qdataFrame(qmodel)[i,j] <<- value
                       }
                     },
                     get_length = function(...) {
                       base:::length(qdataFrame(qmodel))
                     },
                     get_dim = function(...) {
                       base:::dim(qdataFrame(qmodel))
                     },
                     get_names = function(...) {
                       names(qdataFrame(qmodel))
                     },
                     set_names = function(value, ...) {
                       names(qdataFrame(qmodel)) <<- value
                     },
                     get_rownames=function() rownames(qdataFrame(qmodel)),
                     set_rownames=function(value) rownames(qdataFrame(qmodel)) <<- value,
                     get_dimnames=function() dimnames(qdataFrame(qmodel)),
                     set_dimnames=function(value) dimnames(qdataFrame(qmodel)) <<- value,
                     ## editable XXX implement
                     get_editable=function() {},
                     set_editable=function(value, j) {
                       "Set column editable or not"
                     },
                     ## slection
                     get_selected = function(drop=TRUE, ...) {
                       sel <- widget$selectionModel()
                       idxs <- sel$selectedIndexes()
                       if(length(idxs) == 0)
                         return(NULL)
                       out <- sapply(idxs, function(i) c(i$row(), i$column()))

                       rows <- unique(out[1,]) + 1L
                       columns <- unique(out[2,]) + 1L
                       
                       list(rows=rows, columns=columns)
                     },
                     set_selected = function(i, j, ...) {
                       sel <- widget$selectionModel()
                       sel$clearSelection()

                       if(missing(i)) i <- seq_len(get_dim()[1])
                       if(is.null(i)) return()
                         
                       if(is.list(i)) {
                         j <- i[[2]]
                         i <- i[[1]]
                       }
                       if(missing(j)) j <- seq_len(get_dim()[2])

                       ## select i x j: how to vectorize?
                       for(row in i-1) {
                         for(col in j-1) {
                           idx <- qmodel$index(row, col)
                           sel$select(idx, Qt$QItemSelectionModel$Select)
                         }
                       }
                       invisible()
                     },
                     column_decorator=function(handler) {
                       force(handler)
                       f <- function(idx, .self, ...) {
                         handler(column=idx + 1, ..., .self)
                       }
                     },
                     add_handler_column_clicked=function(handler, action=NULL, ...) {
                       add_handler("sectionClicked", handler, action, decorator=.self$column_decorator,
                                   emitter=widget$horizontalHeader())
                     },
                     add_handler_column_double_click=function(handler, action=NULL, ...) {
                       add_handler("sectionDoublrClicked", handler, action, decorator=.self$column_decorator,
                                   emitter=widget$horizontalHeader())
                     },
                     add_handler_column_right_click=function(handler, action=NULL, ...) {
                       XXX()
                     },
                     ## XXX is this the change handler?
                     add_handler_selection_changed=function(handler, action=NULL) {
                       sel <- widget$selectionModel()
                       add_handler("selectionChanged", handler, action, emitter=sel)
                     },
                     add_handler_changed=function(handler, action=NULL, ...) {
                       add_handler("dataChanged", handler, action, emitter=qmodel)
                     }
                     ))
