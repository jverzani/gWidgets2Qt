##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gtable
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gtable guiWidgetsToolkitQt
##' @S3method .gtable guiWidgetsToolkitQt
.gtable.guiWidgetsToolkitQt <-  function(toolkit,
                                         items,
                                         multiple = FALSE,
                                         chosen.col = 1,
                                         icon.col = NULL,
                                         tooltip.col=NULL,
                                         handler = NULL, action = NULL,
                                         container = NULL, ... ) {
  GTable$new(toolkit,
           items=items,
           multiple=multiple,
           chosen.col=chosen.col,
           icon.col = icon.col,
           tooltip.col = tooltip.col,
           handler=handler,
             action=action,
           container=container ,...)
}

##' Class for gtable widget
##'
##' This \code{GTable} class for Qt implements a few additional reference
##' methods:
##' @rdname gWidgets2Qt-package
GTable <- setRefClass("GTable",
                      contains="GWidget",
                      fields=list(
                        items="ANY",
                        chosen_col="integer",
                        icon_col="IntegerOrNULL",
                        tooltip_col="IntegerOrNULL"
                        ),
                      methods=list(
                              initialize=function(toolkit=NULL,
                                items="data.frame",
                                multiple = FALSE,
                                chosen.col = 1,
                                icon.col = NULL,
                                tooltip.col=NULL,
                                handler = NULL, action = NULL,
                                container = NULL, ... ) {

                                
                                ## setup widget
                                widget <<- Qt$QTableView()

                                ## customize widget
                                delegate <- qrTextFormattingDelegate()
                                widget$setItemDelegate(delegate)
                                ## no rows names
                                widget$verticalHeader()$setVisible(FALSE)
                                ## alternate shading
                                widget$setAlternatingRowColors(TRUE)
                                ## stretch last section
                                header <- widget$horizontalHeader()
                                header$setStretchLastSection(TRUE)

                                ## selection mode
                                sel_mode <- ifelse(multiple, "multiple", "single")
                                set_selection_mode(sel_mode)

                                
                                ## select rows only -- not cells
                                widget$setSelectionBehavior(Qt$QAbstractItemView$SelectRows)

                                ## Process icon and tooltip columns, we want indices not names
                                ## we want column index, not name
                                if(is.character(icon.col))
                                  icon.col <- match(icon.col, names(items))
                                if(is.numeric(icon.col))
                                  icon.col <- as.integer(icon.col)
                                
                                if(is.character(tooltip.col))
                                  tooltip.col <- as.integer(match(tooltip.col, names(items)))
                                if(is.numeric(tooltip.col))
                                  tooltip.col <- as.integer(tooltip.col)
                                
                                ## initialize
                                initFields(block=widget,
                                           chosen_col=as.integer(chosen.col),
                                           icon_col = icon.col,
                                           tooltip_col=tooltip.col,
                                           toolkit=toolkit # needed here for gmenu call later
                                           )
                                
                                
                                set_items(items)
                                
                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)
                                
                                callSuper(toolkit)
                              },
                        get_model=function() {
                          "Helper. Get DataFrameModel from proxy"
                          widget$model()$sourceModel()
                        },
                        get_value=function(drop=TRUE, ...) {
                          idx <- get_index()
                          values <- get_items(drop=FALSE)

                          if(drop)
                            values[idx, chosen_col, drop=TRUE]
                          else
                            values[idx, , drop=FALSE]
                        },
                        set_value=function(value, ...) {
                          values <- get_items(drop=FALSE)
                          idx <- match(value, values[, chosen_col, drop=TRUE])
                          if(!any(is.na(idx)))
                            set_index(idx)
                        },
                        get_index=function(...) {
                          idx <- sapply(widget$selectionModel()$selectedRows(), "qinvoke", "row")
                          if(length(idx) == 0)
                            return(integer(0))
                          else
                            return(idx + 1L) # offset

                        },
                        set_index=function(value, ...) {
                          sel_model <- widget$selectionModel()
                          sel_model$clearSelection()
                          sapply(value, function(i) {
                            if(widget$selectionMode == Qt$QAbstractItemView$SingleSelection)
                              sel_model$clearSelection()
                            idx <- get_model()$index(i - 1L, 0L)
                            sel_model$select(idx,  Qt$QItemSelectionModel$Select)
                          })
                        },
                        get_items = function(i, j, ..., drop=TRUE) {
                          "Return items"
                          values <- qdataFrame(get_model())
                          nms <- names(values)
                          ## drop out .decoration and .toolTip
                          nms <- Filter(function(nm) !(grepl(".decoration$", nm) || grepl("^.toolTip$", nm)), nms)
                          values <- values[, nms, drop=FALSE]
                          values[i, j, drop=drop]
                        },
                        get_length=function(...) {
                          get_dim()[2]
                        },
                        get_dim=function(...) {
                          base:::dim(get_items(drop=FALSE))
                        },
                        get_visible=function() {
                          m <- get_dim()[1]
                          if(m >= 1)
                            !sapply(seq_len(m) - 1L, widget$isRowHidden)
                          else
                            logical(0)
                        },
                        set_visible=function(value, ...) {
                          m <- get_dim()[1]
                          if(m >= 1) {
                            value <- rep(value, length.out=m)
                            mapply(widget$setRowHidden, seq_len(m) -1L, !value)
                          }
                        },
                        get_names=function(...) {
                          names(get_items(drop=FALSE))
                        },
                        set_names = function(...) {
                          ## need to avoid .decoration, and .toolTip names
                          ## XXX implement me
                        },
                        hide_names=function(value) {
                          "Change visibility of header. TRUE to hide"
                          widget$horizontalHeader()$setVisible(!value)
                        },
                        set_size=function(value, ...) {
                          "set size also has possibility of column widths"
                          if(is.list(value)) {
                            col_widths <- value$column.widths
                            value$column.widths <- NULL
                            set_column_widths(col_widths)
                            value <- c(width=value$width, height=value$height) # make vector, not list
                          }
                          callSuper(value, ...)
                        },
                        set_column_widths=function(value) {
                          if(length(value) == get_dim()[2]) {
                            cols <-get_view_columns()
                            mapply(gtkTreeViewColumnSetMinWidth, cols, value)
                          }
                        },
                        ## set_items and helpers
                        set_items=function(value, i, j, ...) {
                          "Set items"
                          ## ignore i, j for now
                          if(missing(i) && missing(j)) {
                            l <- extract_pieces(value)
                            model <- qdataFrameModel(l$items, useRoles=TRUE)
                            ## we use a proxy model here in case we want to implement sorting or filtering later on
                            proxy_model <- Qt$QSortFilterProxyModel()
                            proxy_model$setSourceModel(model)
                            widget$setModel(proxy_model)
                            set_icons(l$icons)
                            set_tooltips(l$tooltip)
                          }
                        },
                        extract_pieces=function(items) {
                          "Helper: return list of items:data.frame, icons, tooltips (possibly NULL for last)"
                          ## this is ugly, a better way?
                          if(is.null(icon_col)) {
                            if(is.null(tooltip_col)) {
                              list(items=items, icons=NULL, tooltips=NULL)
                            } else {
                              tooltips <- items[, tooltip_col]
                              values <- items[, -tooltip_col]
                              list(items=values, icons=NULL, tooltips=tooltips)
                            }
                          } else {
                            if(is.null(tooltip_col)) {
                              icons <- items[, icon_col]
                              values <- items[, -icon_col]
                              list(items=values, icons=icons, tooltips=NULL)
                            } else {
                              icons <- items[, icon_col]
                              tooltips <- items[, tooltip_col]
                              values <- items[, -c(icon_col, tooltip_col)]
                              list(items=values, icons=icons, tooltips=tooltips)
                            }
                          }
                        },
                        set_icons = function(icons) {
                          "Helper: set icons if specified as stock icons"
                          if(is.null(icons))
                            return()

                          model <- get_model()
                          first_name <- names(qdataFrame(model))[1]
                          qdataFrame(model)[[sprintf(".%s.decoration", first_name)]] <-
                            lapply(lapply(icons, getStockIconByName), as_qicon)
                        },
                        set_tooltips = function(tooltips) {
                          "Helper: set tooltips for each row"
                          if(is.null(tooltips))
                            return()
                          model <- get_model()
                          first_name <- names(qdataFrame(model))[1]
                          qdataFrame(model)$.toolTip <- tooltips
                        },
                        ## Popup menu
                        default_popup_menu=function(col_index) {
                          "Provide default popup menu (passed to gmenu(..., popup=TRUE))"
                          actions <- list(sort_increasing=
                                          gaction("Sort (increasing)", handler=function(h, ...) {
                                            DF <- get_model()
                                            ind <- order(DF[,col_index], decreasing=FALSE)
                                            DF$setFrame(DF[][ind,])
                                          }),
                                          sort_decreasing=
                                          gaction("Sort (decreasing)", handler=function(h, ...) {
                                            DF <- get_model()
                                            ind <- order(DF[,col_index], decreasing=TRUE)
                                            DF$setFrame(DF[][ind,])
                                          }),
                                          gseparator(),
                                          gaction("Rename column", handler=function(h,...) {
                                            cur_nms <- get_names()
                                            out <- ginput("Rename column", text=cur_nms[col_index], parent=widget)
                                            if(nchar(out)) {
                                              cur_nms[col_index] <- out
                                              set_names(cur_nms)
                                            }
                                          })
                                          )
                          actions
                        },
                        add_popup_menu=function(menulist) {
                          f <- function(...) menulist
                          add_popup(f)
                        },
                        add_popup=function(menu_fun=NULL) {
                          "Add a popup menu to the columns. Function should generate list of actions, ..."
                          if(is.null(menu_fun))
                            menu_fun <- .self$default_popup_menu
                          
                         
                        },
                        remove_popup_menu=function() {
                          "remove popup menu from column headers"
                          
                        },
                        ## 
                        set_selection_mode=function(mode=c("none","single","browse", "multiple", "extended")) {
                          "Helper: Set the selection mode"
                          sel_mode <- switch(match.arg(mode),
                                             "none"   = Qt$QAbstractItemView$NoSelection,
                                             "single" = Qt$QAbstractItemView$SingleSelection,
                                             "browse" = Qt$QAbstractItemView$ContiguousSelection,
                                             "multiple"=Qt$QAbstractItemView$MultiSelection,
                                             "extended"=Qt$QAbstractItemView$ExtendedSelection)
                          widget$setSelectionMode(sel_mode)
                        },
                        ## Handlers
                        add_handler_changed=function(handler, action=NULL, ...) {
                          ## selection changed
                          ## when selection model emits selectionChanged emitter is different
                          decorator <- function(handler) {
                            force(handler)
                            f <- function(selected, deselected, .self, ...) {
                              handler(..., .self)
                            }
                          }
                          
                          add_handler("selectionChanged", handler, action=action, decorator=decorator,
                                      emitter=widget$selectionModel())
                        },
                        add_handler_clicked=function(handler, action, ...) {
                          add_handler_changed(handler, action=action, ...)
                        },
                        add_handler_double_clicked=function(handler, action, ...) {
                         add_handler("doubleClicked", handler, action, ...)
                        }
                    

                        ))
