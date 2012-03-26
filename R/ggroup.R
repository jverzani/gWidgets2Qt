##' @include GContainer.R
NULL


##' toolkit constructor for ggroup
##'
##' @export
##' @rdname gWidgetsQt-undocumented
.ggroup.guiWidgetsToolkitQt <- function(toolkit, horizontal=TRUE, spacing=5, use.scrollwindow=FALSE, container=NULL, ...) {
  GGroup$new(toolkit, horizontal, spacing=spacing, use.scrollwindow=use.scrollwindow, container, ...)
}

GBoxContainer <- setRefClass("GBoxContainer",
                             contains="GContainer",
                             fields=list(
                               horizontal="logical"
                               ),
                             method=list(
                               make_widget=function(horizontal) {
                                 "Widget is a layout. This returns QWidget or Layoutinstance to place into block"
                                 if(horizontal)
                                   widget <<- Qt$QHBoxLayout()
                                 else
                                   widget <<- Qt$QVBoxLayout()
                                 widget$setMargin(0L)
                                 return(widget)
                               },
                               make_scroll_widget=function(horizontal, width=500L, height=2500L) {
                                 "This returns QWidget instance"
                                 ## Need to 
                                 ## QT Docs: Note that You must add the layout of widget
                                 ## before you call this function; if you add it later,
                                 ## the widget will not be visible - regardless of when
                                 ## you show() the scroll area. In this case, you can
                                 ## also not show() the widget later.
                                 
                                 ## widgetResizable : bool This property holds whether
                                 ## the scroll area should resize the view widget. If
                                 ## this property is set to false (the default), the
                                 ## scroll area honors the size of its widget. Regardless
                                 ## of this property, you can programmatically resize the
                                 ## widget using widget()->resize(), and the scroll area
                                 ## will automatically adjust itself to the new size. If
                                 ## this property is set to true, the scroll area will
                                 ## automatically resize the widget in order to avoid
                                 ## scroll bars where they can be avoided, or to take
                                 ## advantage of extra space.
                                 
                                 ## When using a scroll area to display the contents of a
                                 ## custom widget, it is important to ensure that the
                                 ## size hint of the child widget is set to a suitable
                                 ## value. If a standard QWidget is used for the child
                                 ## widget, it may be necessary to call
                                 ## QWidget::setMinimumSize() to ensure that the contents
                                 ## of the widget are shown correctly within the scroll
                                 ## area.
                                 ##
                                 
                                 ## If a scroll area is used to display the
                                 ## contents of a widget that contains child widgets
                               ## arranged in a layout, it is important to realize that
                               ## the size policy of the layout will also determine the
                               ## size of the widget. This is especially useful to know
                               ## if you intend to dynamically change the contents of
                               ## the layout. In such cases, setting the layout's size
                               ## constraint property to one which provides constraints
                               ## on the minimum and/or maximum size of the layout
                               ## (e.g., QLayout::SetMinAndMaxSize) will cause the size
                               ## of the scroll area to be updated whenever the
                               ## contents of the layout changes.

                                 make_widget(horizontal)
                                 
                                 qwidget <- Qt$QWidget()
                                 qwidget$minimumWidth <- width
                                 qwidget$minimumHeight <- height
                                 qwidget$setLayout(widget)

                                 sw <- Qt$QScrollArea()
                                 sw$setWidget(qwidget)

                                 sw
                               },
                               ## svalue (borderwidth, spacing -- which is it...)
                               get_value=function(...) {
                                 widget$spacing
                               },
                               set_value=function(value, ...) {
                                 widget$setSpacing(as.integer(value))
                               },
                             
                               ## layout
                               create_fill = function(fill) {
                                 "Compute fill, takes into account direction of packing unless overridden"
                                 if(is.null(fill))
                                   fill <- ifelse(horizontal, "y", "x")
                                 if(is.logical(fill))
                                   fill <- ifelse(fill, "both", "")
                                 as.character(fill)
                               },
                               add_child = function(child, expand=FALSE, fill=NULL, anchor=NULL, ...) {
                                 "Add child to box container"
                                 
                                 ## @param expand logical or numeric indicating flex value
                                 ## @param fill logical or character (TRUE, FALSE, "x", "y", "both"=TRUE)
                                 ## @param anchor NULL or {-1, 0, 1}^2 value
                                 
                                 qt_child <- getBlock(child)

                                 expand <- ifelse(is.null(expand), FALSE, expand)
                                 stretch <- as.numeric(expand) # expand=FALSE -> 0; TRUE ->1; others as this is a weight
                                 expand <- as.logical(expand)
                                 
                                 fill <- create_fill(fill)

                                 if(is.null(anchor)) {
                                   set_size_policy(qt_child, fill)
                                   widget$addWidget(qt_child, stretch)
                                 } else {
                                   widget$addWidget(qt_child, stretch,   xy_to_align(anchor))
                                 }

                                 update_gui(child)
                                 
                                 ## Internal bookkeeping, add to lists
                                 if(is(child, "GComponent"))
                                   child$set_parent(.self)
                                 children <<- c(children, child)
                               },
                               get_items = function(i, j, ... ,drop=TRUE) {
                                 "Get ith child in list"
                                 out <- children[i]
                               if(drop && length(out) == 1)
                                 out[[1]]
                               else
                                 out
                               }
                               ))


GGroup <- setRefClass("GGroup",
                      contains="GBoxContainer",
                      methods=list(
                        initialize=function(toolkit=NULL,
                          horizontal=TRUE, spacing=5,
                          use.scrollwindow=FALSE,
                          container=NULL, ...) {
                          

                          horizontal <<- horizontal
                          if(use.scrollwindow) {
                            block <<- make_scroll_widget(horizontal, ...)
                          } else {
                            make_widget(horizontal,  ...)
                            block <<- Qt$QWidget()
                            block$setLayout(widget)
                          }
                          set_value(spacing)
                          
                          add_to_parent(container, .self, ...)
                          callSuper(toolkit)
                        }
                        ))

                              
