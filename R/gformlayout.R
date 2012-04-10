##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gformlayout
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gformlayout guiWidgetsToolkitQt
##' @S3method .gformlayout guiWidgetsToolkitQt
.gformlayout.guiWidgetsToolkitQt <-  function(toolkit,
                                             align="left",
                                             spacing=5,
                                             container = NULL, ... ) {
  GFormLayout$new(toolkit,
             align,
             spacing,
             container=container ,...)
}


## a form layout -- really just a table
GFormLayout <- setRefClass("GFormLayout",
                           contains="GContainer",
                           fields=list(
                             align="character",
                             spacing="numeric",
                             lyt="ANY"
                             ),
                           methods=list(
                             initialize=function(toolkit=NULL,
                               align="left", spacing=5,
                               container=NULL,
                               ...) {
                               
                               initFields(align=align,
                                          spacing=spacing
                                          )
                               widget <<- Qt$QFormLayout()
                               block <<- Qt$QWidget()
                               block$setLayout(widget)
                               set_label_alignment(align)
                               
                               add_to_parent(container, .self)
                               callSuper(toolkit, ...)
                             },
                             finalize=function() {
                               ## some cases one needs to call finalize to write table (gWidgetsWWW2)
                             },
                             add_child=function(child, label="", ...) {
                               add_row(label, child, ...)
                             },
                             add_row=function(label, child, ...) {
                               "Add a row at end"

                               widget$addRow(label, getBlock(child))
                               
                               ## bookkeeping
                               if(is(child, "GComponent"))
                                 child$set_parent(.self)
                               ## we keep names here for get_value method
                               nms <- names(children)
                               children <<- c(children, child)
                               names(children) <<- c(nms, label)
                             },
                             get_value=function(...) {
                               "Return list of widget values"
                               sapply(children, svalue, simplify=FALSE) # not get_value, as that doesn't call coerce_with
                             },
                             ## some Qt methods
                             set_label_alignment=function(align) {
                               "Set alignment of label"
                               if(align == "left")
                                 widget$setLabelAlignment(Qt$Qt$AlignLeft)
                               else if(align == "center")
                                 widget$setLabelAlignment(Qt$Qt$AlignCenter)
                             },
                             set_spacing=function(value) widget$setSpacing(as.integer(value)),
                             set_vertical_spacing=function(value) widget$setVerticalSpacing(as.integer(value)),
                             get_count=function() widget$count()
                             ))
                             
