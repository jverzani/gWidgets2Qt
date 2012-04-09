##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gmenu
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gmenu guiWidgetsToolkitQt
##' @S3method .gmenu guiWidgetsToolkitQt
.gmenu.guiWidgetsToolkitQt <-  function(toolkit,
                                           menu.list=list(),
                                           popup=FALSE,
                                           container = NULL,
                                           ... ) {
  if(popup)
    GMenuPopup$new(toolkit, menu.list=menu.list,  ...)
  else
    GMenuBarTop$new(toolkit, menu.list=menu.list, container = container, ...)
}


## MenuBar objects (both MenuBar and Popup)
GMenuBar <- setRefClass("GMenuBar",
                     contains="GWidget",
                     fields=list(
                       menu_list="list"
                       ),
                     methods=list(
                     
                       ## add items
                       add_menu_items=function(sub_menu, items) {
                         sapply(items, function(item) {
                           ## do dispatch based on class
                           if(is(item, "list")) {
                             ## get name by looking up and matching
                             print("add submenu")
                             add_submenu(sub_menu, item, nm=names(Filter(function(x) identical(x, item), items)))
                           } else if(is(item, "GAction")) {
                             print("add action")
                             add_gaction_menuitem(sub_menu, item)
                           } else if(is(item, "GSeparator")) {
                             add_gseparator_menuitem(sub_menu, item)
                           } else if(is(item, "GRadio")) {
                             print("add radio")
                             add_radio_menuitem(sub_menu, item)
                           } else if(is(item, "GCheckbox")) {
                             add_checkbutton_menuitem(sub_menu, item)
                           } else {
                             add_widget_menuitem(sub_menu, item)
                           }
                         })
                         ## sub_menu$show()
                         menu_list <<- items
                       },
                       add_submenu=function(parent_menu, items, nm) {
                         sub_menu <- Qt$QMenu(parent_menu)
                         sub_menu$setTitle(nm)
                         add_menu_items(sub_menu, items)

                         parent_menu$addMenu(sub_menu)
                       },
                       add_gaction_menuitem=function(parent_menu, item) {
                         parent_menu$addAction(getBlock(item))
                       },
                       add_gseparator_menuitem=function(parent_menu, item) {
                         parent_menu$addSeparator()
                       },
                       add_radio_menuitem=function(parent_menu, item) {
                         item$make_items(parent_menu)
                       },
                       add_checkbutton_menuitem=function(parent_menu, item) {
                         parent_menu$addAction(getBlock(item))
                       },
                       add_widget_menuitem=function(parent_menu, item) {
                         "Add an arbitrary widget, though likely not a good thing to do."
                         ## XXX can't do this
                       },
                       clear_menubar=function() {
                         "Clear out menu items"
                        widget$clear()
                       },
                       ##
                       get_value=function( ...) {
                         menu_list
                       },
                       set_value=function(value, ...) {
                         clear_menubar()
                         menu_list <<- value
                         add_menu_items(widget, value)
                       },
                       append_value=function(items) {
                         "Append to menu list"
                         menu_list <<- gWidgets2:::merge.list(menu_list, items)
                         add_menu_items(widget, items)
                       }
                       ))


GMenuBarTop <- setRefClass("GMenuBarTop",
                            contains="GMenuBar",
                            methods=list(
                              initialize=function(toolkit=NULL,
                                menu.list=list(),
                                container=NULL, ...) {

                                if(!is.null(container) && is(container, "GWindow")) {
                                  widget <<- container$widget$menuBar()
                                } else {
                                  stop(gettext("Parent container must be a gwindow instance"))
                                }
                                
                                initFields(block=widget)

                                add_menu_items(widget, menu.list)

                                print("add menubar")
                                container$add_menubar(.self)
                                
                                callSuper(toolkit)
                                
                              }
                              ))
## Popup class
GMenuPopup <- setRefClass("GMenuPopup",
                            contains="GMenuBar",
                            methods=list(
                              initialize=function(toolkit=NULL,
                                menu.list=list(),
                                ...) {

                                widget <<- Qt$QMenu()
                                initFields(block=widget)
                                
                                menu_list <<- menu.list
                                add_menu_items(widget, menu.list)
                                callSuper(toolkit)
                              }
                              ))
