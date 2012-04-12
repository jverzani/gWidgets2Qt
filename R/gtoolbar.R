##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gtoolbar
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gtoolbar guiWidgetsToolkitQt
##' @S3method .gtoolbar guiWidgetsToolkitQt
.gtoolbar.guiWidgetsToolkitQt <-  function(toolkit,
                                              toolbar.list=list(),
                                              style = c("both","icons","text","both-horiz"),
                                              container = NULL,
                                              ... ) {
  GToolBar$new(toolkit,
               toolbar.list=toolbar.list, style=style,
               container=container ,...)
}


## toolbar class
## Oddity: will lower window it is added to.
GToolBar <- setRefClass("GToolBar",
                        contains="GWidget",
                        fields=list(
                          toolbar_list="list"
                          ),
                        methods=list(
                          initialize=function(toolkit=NULL,
                            toolbar.list=list(),
                            style = c("both", "icons", "text", "both-horiz"),
                            container = NULL,
                            ...) {

                            QtToolButtonStyle <-
                              list("both"=Qt$Qt$ToolButtonTextUnderIcon,
                                   "icons"=Qt$Qt$ToolButtonIconOnly,
                                   "text"=Qt$Qt$ToolButtonTextOnly,
                                   "both-horiz"=Qt$Qt$ToolButtonTextBesideIcon)
                            
                            
                            widget <<- Qt$QToolBar()
                            widget$setToolButtonStyle(QtToolButtonStyle[[match.arg(style)]])
                            
                            initFields(block=widget,
                                       toolbar_list=list()
                                       )

                            add_toolbar_items(toolbar.list)

                            if(!is.null(container) && is(container, "GWindow"))
                              add_to_parent(container, .self, ...)
                            
                            callSuper(toolkit)
                          },
                          add_toolbar_items=function(items) {
                            "Map a toolbar list, a named list of gaction items or gseparator items"
                            sapply(items, function(item) {
                              ## do dispatch based on class
                              if(is(item, "GAction"))
                                add_gaction_toolitem(item)
                              else if(is(item, "GSeparator"))
                                add_gseparator_toolitem(item)
                              else
                                add_widget_toolitem(item)
                            })
##                            widget$show()
                            toolbar_list <<- gWidgets2:::merge.list(toolbar_list, items)
                          },
                          add_gseparator_toolitem=function(obj) {
                            "Helper to add a separator"
                            widget$addSeparator()
                          },
                          add_gaction_toolitem=function(obj) {
                            "Helper to add a gaction item"
                            widget$addAction(getBlock(obj))
                          },
                          add_widget_toolitem=function(obj) {
                            "Add a widget to the toolbar"
                            child <- getBlock(obj)
                            if(is(child, "QWidget"))
                              widget$addWidget(child)
                            else if(is(child, "QAction"))
                              widget$addAction(child)
                          },
                          clear_toolbar=function() {
                            "Clear toolbar items"
                            x <- widget$clear()
                            toolbar_list <<- list()
                            widget$hide()
                          },
                          get_value=function( ...) {
                            toolbar_list
                          },
                          set_value=function(value, ...) {
                            "Clear toolbar, add anew"
                            clear_toolbar()
                            add_toolbar_items(value)
                          }
                          ))

