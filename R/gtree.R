##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gtree
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gtree guiWidgetsToolkitQt
##' @S3method .gtree guiWidgetsToolkitQt
.gtree.guiWidgetsToolkitQt <-  function(toolkit,
                                           offspring = NULL, offspring.data = NULL,
                                           chosen.col = 1, offspring.col=2, icon.col=NULL, tooltip.col=NULL,
                                           multiple = FALSE,
                                           handler = NULL,action = NULL, container = NULL, ... ) {
  GTree$new(toolkit,
            offspring=offspring, offspring.data=offspring.data,
            chosen.col=chosen.col, offspring.col=offspring.col, icon.col=icon.col, tooltip.col=tooltip.col,
            multiple=multiple,
            handler = handler,action = action, container = container, ...)
}


## Base class
##
## Base class of gtree and gvarbrowser
GTreeBase <- setRefClass("GTreeBase",
                         contains="GWidget",
                         methods=list(
                           init_model=function(DF) {
                             "Initialize model. Need to add in number of column and names"
                             model <- Qt$QStandardItemModel(rows=0, columns=ncol(DF))
                             model$setHorizontalHeaderLabels(c("", names(DF)))
                             widget$setModel(model)
                             model$setParent(widget) # avoid early GC
                           },
                           path_from_index=function(idx, ...) {
                             "convert index to item, then call path_from_item"
                             item <- widget$model()$itemFromIndex(idx)
                             path_from_item(item)
                           },
                           path_from_item=function(item, return_path=TRUE, return_index=TRUE) {
                             "Return list(path,index) from item"
                             path <- c()
                             indices <- c()
                             model <- widget$model()
                             
                             ## walk backward to beginning
                             par <- item$parent()
                             while(!is.null(par)) {
                               path <- c(item$text(), path)
                               indices <- c(model$indexFromItem(item)$row()  + 1L, indices)
                               item <- par
                               par <- item$parent()
                             }
                             path <- c(item$text(), path)
                             indices <- c(model$indexFromItem(item)$row() + 1L, indices)
                             
                             l <- list()
                             if(return_path) l$path <- path
                             if(return_index) l$indices <- indices
                             return(l)
                           },
                           
                           
                           set_selection_mode=function(mode=c("none", "single", "browse", "multiple", "extended")) {
                             "Helper: Set the selection mode (from gtable)"
                             sel_mode <- switch(match.arg(mode),
                                                "none"   = Qt$QAbstractItemView$NoSelection,
                                                "single" = Qt$QAbstractItemView$SingleSelection,
                                                "browse" = Qt$QAbstractItemView$ContiguousSelection,
                                                "multiple"=Qt$QAbstractItemView$MultiSelection,
                                                "extended"=Qt$QAbstractItemView$ExtendedSelection)
                             widget$setSelectionMode(sel_mode)
                           },
                           set_multiple=function(value) {
                             set_selection_mode(ifelse(value, "multiple", "single"))
                           },
                           ## main interface
                           get_value=function(drop=TRUE,...) {
                             "Return path (by chosen col)"
                             sel_model <- widget$selectionModel()
                             selected_rows <- sel_model$selectedRows() # a list of QModelIndex
                             if(length(selected_rows) == 0)
                               return(NULL) # no selection XXX what return value? character(0)?
                             out <- lapply(selected_rows, .self$path_from_index)
                             out <- lapply(out, function(i) i$path)

                             drop <- ifelse(is.null(drop), FALSE, drop)
                             if(drop)
                               out <- lapply(out, function(i) tail(i, n=1))
                             
                             if(length(out) == 1)
                               return(out[[1]])
                             else
                               return(out)
                           },
                           set_value=function(value, ...) {
                             "open path, set via match"
                             ## this is trickier than it look
                             
                           },
                           get_index = function(drop=FALSE, ...) {
                             "get  index as integer vector"
                             ## Shares *much* code with get_value (only i$indices below). Tighten up
                             sel_model <- widget$selectionModel()
                             selected_rows <- sel_model$selectedRows() # a list of QModelIndex
                             if(length(selected_rows) == 0)
                               return(NULL) # no selection XXX what return value? character(0)?
                             out <- lapply(selected_rows, .self$path_from_index)
                             out <- lapply(out, function(i) i$indices)

                             drop <- ifelse(is.null(drop), FALSE, drop)
                             if(drop)
                               out <- lapply(out, function(i) tail(i, n=1))
                             
                             if(length(out) == 1)
                               return(out[[1]])
                             else
                               return(out)
                           },
                           set_index = function(value,...) {
                             "open to specifed index, if possible"
                             ## value may be a list, here we recurse if it is
                             if(is.list(value)) sapply(value, set_index)

                             ## value is index vector
                             model <- widget$model()
                             node <- model$invisibleRootItem()
                             while(length(value)) {
                               child <- node$child(value[1] - 1)
                               if(is.null(child))
                                 return()   # nothing to do, doesn't match
                               ## open
                               if(length(value) > 1) {
                                 idx <- model$indexFromItem(child)
                                 if(child$hasChildren())
                                   widget$expand(idx)
                               }
                               
                               node <- child
                               value <- value[-1]
                             }

                             ## select node
                             item <- model$indexFromItem(node)
                             sel_model <- widget$selectionModel()
                             sel_model$select(item,
                                              Qt$QItemSelectionModel$Select |
                                              Qt$QItemSelectionModel$Rows |
                                              Qt$QItemSelectionModel$Clear
                                              )
                             
                             
                           },
                           get_items = function(i, j, ..., drop=TRUE) {
                             "Get items in the selected row"
                             out <- get_value(drop=FALSE)
                             ## XXX clarify what this does ...
                             out
                           },
                           set_items = function(value, i, j, ...) {
                             stop(gettext("One sets items at construction through the x argument of offspring function"))
                           },
                           get_names=function() {
                             "Get column names"
                             model <- widget$model()
                             sapply(1:model$columnCount(), function(i) model$horizontalHeaderItem(i-1)$data(0))
                           },
                           set_names=function(value) {
                             ## Should be set via offspring, but it isn't so hard:
                             widget$model()$setHorizontalHeaderLabels(value)
                           },
                           
                           ## Some extra methods
                           clear_selection=function() {
                             sel_model <- widget$selectionModel()
                             sel_model$clear()
                           }
                           ))


## C;ass for GTree and the whole offspring parameterization
GTree <- setRefClass("GTree",
                     contains="GTreeBase",
                     fields=list(
                       chosen_col="IntegerOrNULL",
                       offspring_col="IntegerOrNULL",
                       icon_col="IntegerOrNULL",
                       tooltip_col="IntegerOrNULL",
                       #
                       offspring_data="ANY",
                       offspring="function"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         offspring = NULL, offspring.data = NULL,
                         chosen.col = 1, offspring.col=2, icon.col=NULL, tooltip.col=NULL,
                         multiple = FALSE,
                         handler=NULL, action=NULL, container=NULL, ...) {

                         ## create view and style
                         widget <<-  Qt$QTreeView()
                         widget$setEditTriggers(Qt$QAbstractItemView$NoEditTriggers)
                         widget$setSelectionBehavior(Qt$QAbstractItemView$SelectRows)
                         
                         ## call offspring to get data frame
                         items <- offspring(c(), offspring.data)

                         ## we want column index, not name
                         .character_to_index <- function(val, x) {
                           if(is.character(val)) {
                             if(is.element(val, x))
                               val <- match(val, x)
                             else
                               val <- NULL
                           }
                           if(is.numeric(val))
                             val <- as.integer(val)
                           val
                         }
                         icon.col <- .character_to_index(icon.col, names(items))
                         tooltip.col <- .character_to_index(tooltip.col, names(items))
                         offspring.col <- .character_to_index(offspring.col, names(items))
                         chosen.col <- .character_to_index(chosen.col, names(items))

                         initFields(block=widget,
                                    offspring=offspring,
                                    chosen_col=chosen.col,
                                    offspring_col=offspring.col,
                                    icon_col = icon.col,
                                    tooltip_col=tooltip.col,
                                    offspring_data=offspring.data,
                                    change_signal="activated", # or itemActivated for single?
                                    default_expand=TRUE,
                                    default_fill=TRUE
                                    )

                      

                         set_multiple(multiple)

                         
                         init_model()
                         add_offspring(character(0), NULL)

                         ## handlers for expand and contract
                         qconnect(widget, "expanded", function(idx) {
                           item <- widget$model()$itemFromIndex(idx)
                           assign("item", item, .GlobalEnv)
                           ## remove children (added one to make expandable)
                           item$setRowCount(0L)
                           ## XXX adjust this...
                           add_offspring(path_from_item(item)$path, item)
                         })
                         qconnect(widget, "collapsed", function(idx) {
                           item <- widget$model()$itemFromIndex(idx)
                           ## don't need to do anything?
                         })

                         
                         add_to_parent(container, .self, ...)
                         
                         handler_id <<- add_handler_changed(handler, action)
                         
                         callSuper(toolkit)
                       },
                       init_model=function(DF) {
                         "Initialize model. Need to add in number of column and names"
                         if(missing(DF))
                           DF <- offspring_pieces(character(0))$DF
                         callSuper(DF)
                       },
                        add_offspring=function(path, item=NULL) {
                             "Helper: add offspring for the path at the item"
                             
                             if(is.null(item))
                               item <- widget$model()$invisibleRootItem()
                             
                             pieces <- offspring_pieces(path)
                             DF <- pieces$DF
                             if(nrow(DF) > 0) {
                               mapply(.self$add_row, list(item), lapply(1:nrow(DF), function(i) DF[i,]),
                                      pieces$offspring,
                                      if(is.null(pieces$icon)) list(NULL) else pieces$icon,
                                      if(is.null(pieces$tooltip)) list(NULL) else pieces$tooltip)
                             }
                           },
                       add_row=function(parent_item, DF_row, offspring=FALSE, icon=NULL, tooltip=NULL) {
                         "Helper: function to add a row. Called by add_offspring"
                         ## values
                         l <- lapply(as.character(unlist(DF_row)), Qt$QStandardItem)
                         parent_item$appendRow(l)
                         
                         ## icon
                         if(!is.null(icon) && !is.null(icon <- getStockIconByName(icon)))
                           l[[1]]$setIcon(as_qicon(icon))
                         
                         ## tooltip
                         if(!is.null(tooltip))
                           l[[1]]$setToolTip(as.character(tooltip))
                         
                         if(offspring) {
                           ## set bogus child to make trigger icon appear
                           l[[1]]$setChild(0,0, Qt$QStandardItem())
                         }
                       },
                       offspring_pieces=function(path) {
                         "Helper: Compute offspring, return pieces DF, offspring, icon, tooltip in a list"
                         os <- offspring(path, offspring_data)
                         the_offspring <- icon <- tooltip <- NULL
                         if(!is.null(offspring_col))
                           the_offspring <- os[[offspring_col]]
                         if(!is.null(icon_col))
                            icon <- os[[icon_col]]
                         if(!is.null(tooltip_col))
                           tooltip <- os[[tooltip_col]]

                         idx <- unlist(list(offspring_col, icon_col, tooltip_col))
                         if(is.null(idx))
                           DF <- os
                         else
                           DF <- os[-idx]

                         list(DF=DF, offspring=the_offspring, icon=icon, tooltip=tooltip)
                       },
                       update_widget=function(...) {
                         "Update base of widget, reopen selected paths if possible"
                         cur_selection <- as.list(get_index())
                         model <- widget$model()
                         model$setRowCount(0)
                         add_offspring(character(0), NULL)
                         sapply(cur_selection, set_index)
                       }
                       ))




GTreeDataFrame <- setRefClass("GTreeDataFrame",
                              contains="GTreeBase",
                              fields=list(
                                idx="numeric"
                                ),
                              methods=list(
                                initialize=function(DF, INDICES,
                                  multiple = FALSE,
                                  handler=NULL, action=NULL, container=NULL, ...) {
                                  
                                  ## check that INDICES are numeric or in names
                                  if(missing(INDICES))
                                    stop(gettext("INDICES are required. May be of length 1 or more"))
                                  if(is.numeric(INDICES)) {
                                    INDICES <- as.integer(INDICES)
                                  } else if(is.character(INDICES)) {
                                    if(!all(INDICES %in% names(DF)))
                                      stop(gettext("INDICES are numeric index or subset of names"))
                                    INDICES <- match(INDICES, names(DF))
                                  } else {
                                    stop(gettext("INDICES are numeric index or subset of names"))
                                  }
                                  idx <<- as.integer(INDICES)
                                  
                                  ## make tree widget
                                  ## create view and style
                                  widget <<-  Qt$QTreeView()
                                  widget$setEditTriggers(Qt$QAbstractItemView$NoEditTriggers)
                                  widget$setSelectionBehavior(Qt$QAbstractItemView$SelectRows)
                                  initFields(block=widget,
                                             change_signal="activated")
                                  set_multiple(multiple)

                                  items <- DF[-idx]
                                  init_model(items)
                                  
                                  populate_tree(DF, idx)

                                  
                                  add_to_parent(container, .self, ...)
                         
                                  handler_id <<- add_handler_changed(handler, action)
                         
                                  callSuper(toolkit)
                                },
                                populate_tree=function(DF, ind) {
                                  l <- split(DF, DF[[ind[1]]])
                                  mapply(.self$populate_level, names(l), l, list(ind[-1]), list(root_node()))
                                },
                                root_node=function() {
                                  "Return root node"
                                  widget$model()$invisibleRootItem()
                                },
                                populate_level=function(nm, DF, ind, node) {
                                  ## what to do. If ind has values, we recurse
                                  if(length(ind) > 0) {
                                    item <- Qt$QStandardItem(nm)
                                    node$appendRow(item)
                                    lst <- split(DF, factor(DF[[ ind[1] ]]))
                                    mapply(.self$populate_level, names(lst), lst, list(ind[-1]), list(item))
                                  } else {
                                    sapply(seq_len(nrow(DF)), function(i) {
                                      values <- sapply(DF[i,-idx, drop=FALSE], as.character)
                                      items <- lapply(c(nm, values), Qt$QStandardItem)
                                      node$appendRow(items)
                                    })
                                  } 
                                }
                                ))
                                  
