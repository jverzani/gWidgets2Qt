##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::glayout
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .glayout guiWidgetsToolkitQt
##' @S3method .glayout guiWidgetsToolkitQt
.glayout.guiWidgetsToolkitQt <-  function(toolkit,
                                             homogeneous = FALSE, spacing = 10,
                                             container = NULL, ... ) {
  GLayout$new(toolkit=toolkit, homogeneous=homogeneous, spacing=spacing, container = container, ...)
}


## layout class
GLayout <- setRefClass("GLayout",
                       contains="GContainer",
                       fields=list(
                         child_positions="list",
                         homogeneous="logical"
                         ),
                       methods=list(
                         initialize=function(toolkit=NULL,
                           homogeneous = FALSE, spacing = 10,
                           container = NULL, ... 
                           ) {
                           
                           initFields(widget=Qt$QGridLayout(),
                                      block = Qt$QWidget(),
                                      child_positions=list(),
                                      homogeneous=homogeneous
                                      )
                           block$setLayout(widget)

                           widget$setContentsMargins(1,1,1,1) # area around widget
                           set_spacing(spacing)
                           
                           add_to_parent(container, .self, ...)

                           callSuper(toolkit)
                         },
                         add_child=function(...) {
                           ## XXX does nothing, adding done in [<- side
                         },
                         set_spacing=function(value) {
                           widget$setSpacing(as.integer(value)) # does both horizontal and vertical
                         },
                         get_dim=function(...) {
                           "current size of table"
                           c(nrow=widget$rowCount(), ncol=widget$columnCount())
                         },
                         get_items = function(i, j, ..., drop=TRUE) {
                           ## make matrix, then extract
                           ## XXX This should sit in gWidgets2 -- it goes for all toolkits
                           d <- get_dim()
                           m <- matrix(nrow=d[1], ncol=d[2])
                           for(index in seq_along(child_positions)) {
                             item <- child_positions[[index]] 
                             for(ii in item$x)
                               for(jj in item$y) {
                                 m[ii,jj] <- index
                               }
                           }
                           widgets <- sapply(as.vector(m), function(ii) {
                             if(is.na(ii))
                               NA
                             else
                               child_positions[[ii]]$child
                           })
                           widgets <- matrix(widgets, ncol=d[2])
                           out <- widgets[i,j, drop=drop]
                           if(length(out) == 1 && drop)
                             out <- out[[1]]
                           out
                         },
                         set_items = function(value, i, j, expand=FALSE, fill=FALSE, anchor=NULL, ...) {
                           "Main method to add children"
                           ## @param expand logical or numeric
                           ## @param fill logical or "x", "y", or "both"
                           ## @param anchor NULL of in {-1, 0, 1}^2

                           if(missing(j)) {
                             warning(gettext("glayout: [ needs to have a column specified."))
                             return()
                           }

                           if(missing(i))
                             i <- get_dim()[1] + 1

                           
                           if(is.character(value)) {
                             value <- glabel(value, toolkit=toolkit)
                           }
                           
                           
                           ## widgets
                           child <- getBlock(value)
                           
                           ## process anchor, expand fill
                            if(!is.null(fill) || !is.null(anchor))
                              expand <- TRUE

                           align <- Qt$Qt$AlignTop | Qt$Qt$AlignLeft # default alignment

                           ## anchor or fill, share with ggroup?
                           if(is.null(anchor)) {
                             set_size_policy(child, fill)
                           } else {
                             ## anchor non NULL
                             align <- xy_to_align(anchor)
                           }

                           if(homogeneous) {
                             ## stretch rows
                             stretch <- 1
                             for(col in seq(min(i), min(i) + length(i) - 1))
                               widget$setColumnStretch(col-1, stretch)
                             for(row in seq(min(j), min(j) + length(j) - 1))
                               widget$setRowStretch(row-1, stretch)
                           }

                           widget$addWidget(child, fromRow=as.integer(min(i)-1), fromColumn=as.integer(min(j)-1),
                                            rowSpan=as.integer(length(i)), columnSpan=as.integer(length(j)), align)

                           update_gui(child)
                             
                           ## Internal bookkeeping, add to lists
                           if(is(value, "GComponent"))
                             value$set_parent(.self)
                           children <<- c(children, value)
                           ## store for [ method
                           l <- child_positions
                           l[[as.character(length(l) + 1)]] <- list(x=i, y=j, child=value)
                           child_positions <<- l
                         }
                         ))

