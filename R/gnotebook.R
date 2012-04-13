##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gnotebook
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gnotebook guiWidgetsToolkitQt
##' @S3method .gnotebook guiWidgetsToolkitQt
.gnotebook.guiWidgetsToolkitQt <-  function(toolkit,
                                            tab.pos = 3, 
                                            container = NULL, ... ) {
  GNotebook$new(toolkit, tab.pos, 
                    container = container, ...)
}

## parent class for Notebook and stackedwidget
GCardContainer <- setRefClass("GCardContainer",
                              contains="GContainer",
                              methods=list(
                                get_value=function( ...) {
                                  widget$currentIndex + 1L
                                },
                                set_value=function(value, ...) {
                                  n <- get_length()
                                  value <- max(1, min(value,n))
                                  widget$setCurrentIndex(value - 1L)
                                },
                                get_index = function(...) {
                                  get_value()
                                },
                                set_index = function(value,...) {
                                  set_value(value)
                                },
                                get_length = function(...) {
                                  "Number of pages"
                                  widget$count
                                },
                                remove_page_by_index=function(i) {
                                  child <- get_items(i)
                                  remove_child(child)
                                },
                                remove_current_page = function() {
                                  remove_page_by_index(get_index())
                                },
                                ## handlers
                                add_handler_changed=function(handler, action=NULL, ...) {
                                "A tab changed"
                                if(!is_handler(handler)) return()
                                decorator <- function(FUN) {
                                  force(FUN)
                                  f <- function(tab, .self, ...) {
                                    FUN(page.no=tab + 1L, ..., .self)
                                  }
                                  f
                                }
                                add_handler("currentChanged", handler, action=action, decorator=decorator, ...)
                              }
                             ))

GNotebook <- setRefClass("GNotebook",
                         contains="GCardContainer",
                         methods=list(
                           initialize=function(toolkit=NULL, tab.pos=3, 
                             container=NULL, ...) {


                             block <<- widget <<- Qt$QTabWidget()

                             
            
                             ## tab placement: 1,2,3,4 -> 3,0,2,1
                             RtoTabPositionEnum <- as.integer(c(1,2,0,3))
                             widget$setTabPosition(RtoTabPositionEnum[tab.pos])

                             ## In Qt we close all tabs or none. This is different than gWidgets2
                             ## where this info is passed to the add method
                             ##
                             close_buttons <- getWithDefault(list(...)$close.buttons, TRUE)
                             if(close_buttons) {
                               widget$setTabsClosable(TRUE)
                               ## connect to signal
                               qconnect(widget, "tabCloseRequested", function(index) {
                                 widget$removeTab(index)
                               })
                             }
                             widget$setUsesScrollButtons(TRUE)
                             
                             add_to_parent(container, .self, ...)
                             
                             callSuper(toolkit)
                           },
                           get_names = function(...) {
                             n <- get_length()
                             if(n > 0)
                               vals <- sapply(1:n, function(i) widget$tabText(i-1))
                             else
                               vals <- character(0)
                             return(vals)
                           },
                           set_names = function(value, ...) {
                             n <- get_length()
                             if(length(value) != n)
                               stop(gettext("New names for notebook must have proper length"))
                             
                             sapply(1:n, function(i) {
                               widget$setTabText(i-1, as.character(value[i]))
                             })

                           },
                           add_child=function(child, label="", index=NULL, close.button=FALSE, tooltip=NULL, ...) {
                             
                             ## in ... we have many possibilies
                             ## label -- for setting label  (also look for name)
                             ## index for setting the index of page to add
                             
                             qt_child <- getBlock(child)

                             ## if(close.button)
                             ##   message(gettext("Unable to add close.buttons individually"))

                             label <- paste(label, collapse="\n")
#                             if(!is.character(label))
#                               label = svalue(label)     # now a character

                             if(is.null(index) || !is.numeric(index))
                               index <- get_length() + 1
                             index <- min(get_length() + 1, max(1, index))
                             
                             ## add drop motion for labels
                             ## add at index
                             widget$insertTab(index-1, qt_child, paste(label, collapse="\n"))

                             set_index(index)
                             
                             if(!is.null(tooltip))
                               set_tab_tooltip(index, tooltip)
                             

                             child_bookkeeping(child)
                           },
                           set_tab_tooltip=function(index, tip) {
                             "Set a tooltip per tab"
                             widget$setTabToolTip(index - 1L, tip)
                           },
                           set_tabs_closable=function(value) {
                             "Can override close buttons en masse"
                             widget$setTabsClosable(as.logical(value))
                           },
                           remove_child=function(child) {
                             children <<- Filter(function(x) !identical(x, child), children)
                             child$set_parent(NULL)
                             getBlock(child)$hide()
                             widget$removeTab(widget$indexOf(getBlock(child)))
                           },
                           remove_current_page=function() {
                             "dispose is remove current page"
                             remove_child(get_items(get_index()))
                           }
                           ))

