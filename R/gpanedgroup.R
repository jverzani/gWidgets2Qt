##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gpanedgroup
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gpanedgroup guiWidgetsToolkitQt
##' @S3method .gpanedgroup guiWidgetsToolkitQt
.gpanedgroup.guiWidgetsToolkitQt <-  function(toolkit,
                                                horizontal = TRUE, 
                                                container = NULL, ... ) {
  GPanedGroup$new(toolkit,
           horizontal=horizontal, 
           container = container, ...)
}


## main class
GPanedGroup <- setRefClass("GPanedGroup",
                            contains="GContainer",
                           fields=list(
                             horizontal="logical"
                             ),
                            methods=list(
                              initialize=function(toolkit=NULL,
                                horizontal=TRUE,
                                container=NULL, ...) {

                                initFields(widget=Qt$QSplitter(),
                                           horizontal=horizontal
                                           )
                                block <<- widget

                                set_horizontal(horizontal)

                                add_to_parent(container, .self, ...)
                                callSuper(toolkit)
                              },
                              set_horizontal=function(value) {
                                "Set orientation to horizontal (TRUE) or vertical (FALSE)"
                                widget$setOrientation(ifelse(as.logical(value), Qt$Qt$Horizontal, Qt$Qt$Vertical))
                              },
                              get_value = function(...) {
                                "get sash position"
                                sz <- widget$sizes()
                                sz[1]/sum(sz)
                              }, 
                              set_value = function(value, ...) {
                                "Set sash position"
                                szs <- widget$sizes()
                                if(is.integer(value)) {
                                  left <- value
                                } else {
                                  left <- floor(ifelse(horizontal, szs[2], szs[1]) * value)
                                }
                                right <- ifelse(horizontal, szs[2], szs[1]) - left 
                                widget$setSizes(as.integer(c(left, right)))
                              },
                              get_items = function(i, j, ..., drop=TRUE) {
                                children[[i, drop=drop]]
                              },
                              get_length = function() {
                                length(children)
                              },
                              add_child=function(child, expand=NULL, fill=NULL, anchor=NULL, ...) {
                                "Add one of two possible children"
                                n <- get_length()
                                if(n >= 2) {
                                  message(gettext("Already have two children. Remove one via remove_child?"))
                                  return()
                                }

                                qt_child <- getBlock(child)
                                qt_child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                                       Qt$QSizePolicy$Expanding)
                                widget$addWidget(qt_child)
                                
                                child_bookkeeping(child)
                              }
                              ))

