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


##' Base class
##'
##' For \code{GTree}, there are extra reference methods:
##' \code{set_multiple} to set whether multiple or single selection is
##' being used.
GTree <- setRefClass("GTree",
                     contains="GWidget",
                     fields=list(
                       chosen_col="IntegerOrNULL",
                       offspring_col="IntegerOrNULL",
                       icon_col="IntegerOrNULL",
                       tooltip_col="IntegerOrNULL",
                       offspring_data="ANY",
                       offspring="function",
                       multiple="logical",
                       itemsWithChildren="list"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         offspring = NULL, offspring.data = NULL,
                         chosen.col = 1, offspring.col=2, icon.col=NULL, tooltip.col=NULL,
                         multiple = FALSE,
                         handler=NULL, action=NULL, container=NULL, ...) {

                         widget <<-  Qt$QTreeView()
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
                                    change_signal="itemDoubleClicked", # or itemActivated for single?
                                    default_expand=TRUE,
                                    default_fill=TRUE,
                                    toolkit=toolkit # needed here for gmenu call later
                                    )

                      

                         set_multiple(multiple)

                         
                         init_model()
                         add_offspring(character(0), NULL)

                         ## handlers
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
                       init_model=function() {
                         DF <- offspring_pieces(character(0))$DF
                         model <- Qt$QStandardItemModel(rows=0, columns=ncol(DF))
                         model$setHorizontalHeaderLabels(names(DF))
                         widget$setModel(model)
                         model$setParent(widget) # avoid early GC
                       },
                       path_from_item=function(item) {
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
                         
                         return(list(path=path, indices=indices))
                       },
                       add_offspring=function(path, item) {
                         "add offspring"

                         if(is.null(item))
                           item <- widget$model()$invisibleRootItem()
                         
                         pieces <- offspring_pieces(path)
                         DF <- pieces$DF
                         if(nrow(DF) > 0) {
                           mapply(.self$add_row, list(item), lapply(1:nrow(DF), function(i) DF[i,]),
                                  pieces$offspring,
                                  pieces$icon,
                                  pieces$tooltip)
                         }
                       },
                       add_row=function(parent_item, DF_row, offspring=FALSE, icon=NULL, tooltip=NULL) {
                         ## values
                         cat("add_row")
                         print(parent_item)
                         print(DF_row)
                         print("===")

                         
                         l <- lapply(as.character(unlist(DF_row)), Qt$QStandardItem)
                         parent_item$appendRow(l)

                         ## icon
                         if(!is.null(icon <- getStockIconByName(icon)))
                           l[[1]]$setIcon(gWidgets2Qt:::as_qicon(icon))

                         ## tooltip
                         if(!is.null(tooltip))
                           l[[1]]$setToolTip(tooltip)
                         
                         if(offspring) {
                           ## set bogus child to make trigger icon appear
                           l[[1]]$setChild(0,0, Qt$QStandardItem())
                         }
                       },
                       offspring_pieces=function(path) {
                         "Compute offspring, return pieces DF, offspring, icon, tooltip in a list"
                         os <- offspring(path, offspring.data)
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
                       ## we need to keep track of which items have children,
                       ## as there is no way within Qt to do so except childCount
                       ## and we don't have the count until we expand
                       itemHasChild = function(item) {
                         itemsWithChildren[[digest(item)]] <<- TRUE
                       },
                       itemHasNoChild = function(item) {
                         itemsWithChildren[[digest(item)]] <<- NULL
                       },
                       doesItemHaveChildren = function(item) {
                         id <- digest(item)
                         !is.null(itemsWithChildren[[id]])
                       },
                       ## helper to make a child item
                       makeChildItem=function(data, hasChild=FALSE, icon=NULL) {
                         ##
                         if(length(data) == 0) {
                           cat("Need some data")
                           data=list("nothing here")
                         }

                         data <- as.list(data)
                         
                         if(icon == "")
                           icon <- NULL
                         item <- list(icon=icon,
                                      data=data,
                                      children=list()
                                      )
                         
                         if(is.na(hasChild) || !hasChild)
                           item$children <- NULL
                         
                         class(item) <- c("childItem", class(item))
                         item
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
                         if(value) {
                           set_selection_mode("multiple")
                           multiple <<- TRUE
                         } else {
                           set_selection_mode("single")
                           multiple <<- FALSE
                         }
                       },
                           
                       ## main methods
                       get_value=function(i, drop=TRUE,...) {
                         "Return path (by chosen col)"
                       },
                       set_value=function(value, ...) {
                         "open path, set via match"
                         ## this is trickier than it look
                         
                       },
                       get_index = function(...) {
                         "get path index as integer vector"
                       },
                       set_index = function(value,...) {
                         "open to specifed index, if possible"
                         ## value may be a list
                       },
                       get_items = function(i, j, ..., drop=TRUE) {
                         "Get items in the selected row"
                       },
                       set_items = function(value, i, j, ...) {
                         stop(gettext("One sets items at construction through the x argument of offspring function"))
                       },
                       get_names=function() {
                         
                       },
                       set_names=function(value) {
                       },
                       update_widget=function(...) {
                         "Update base of widget, reopen selected paths if possible"
                       },
                       ##
                       add_handler_changed=function(handler, action=NULL, ...) {
                         add_handler("XXX", handler, action=action, ...)
                       },
                       ## Some extra methods
                       clear_selection=function() {

                       }
                       ))

