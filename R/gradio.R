##' @include gcheckboxgroup.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gradio
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gradio guiWidgetsToolkitQt
##' @S3method .gradio guiWidgetsToolkitQt
.gradio.guiWidgetsToolkitQt <-  function(toolkit,
                                            items,selected=1, horizontal=FALSE, handler=NULL,
                                            action=NULL, container=NULL, ..., parent=NULL
                                            ) {

  if(!is.null(parent)) {
    ## A menu or toolbar item
    GRadioMenuItems$new(toolkit, items, selected,
                        handler, action, parent, ...)
  } else {
    GRadioButtons$new(toolkit, items, selected, horizontal,
                      handler, action, container, ...)
  }
}

GRadio <- setRefClass("GRadio",
                      contains="GButtonGroupWidget")


  
## radio button class
GRadioButtons <- setRefClass("GRadioButtons",
                      contains="GRadio",
                      methods=list(
                        initialize=function(toolkit, items, selected, horizontal,
                          handler, action, container, ...) {

                          
                          if(horizontal)
                            widget <<-  Qt$QHBoxLayout()
                          else
                            widget <<- Qt$QVBoxLayout()
                          block <<- Qt$QWidget()
                          block$setLayout(widget)
                          
                          constructor <<- Qt$QRadioButton
                          
                          button_group <<- Qt$QButtonGroup()
                          button_group$setExclusive(TRUE)
                          
                          set_items(value=items)
                          set_index(selected)
                          add_to_parent(container, .self, ...)
                          
                          handler_id <<- add_handler_changed(handler, action)
                          
                          callSuper(toolkit)

                        },
                        get_index=function(...) {
                          btns <- button_group$buttons()
                          which(sapply(btns, function(i) i$checked))
                        },
                        set_index=function(value, ...) {
                          if(is.logical(value))
                            value <- which(value) # numeric now
                          btns <- button_group$buttons()
                          btns[[value]]$setChecked(TRUE)
                        }
                        ))


## This is used in menubars
GRadioMenuItems <- setRefClass("GRadioMenuItems",
                               contains="GRadio",
                               fields=list(
                                 items="ANY"
                                 ),
                               methods=list(
                                 initialize=function(toolkit, items, selected,
                                   handler, action, parent, ...) {

                                   widget <<- Qt$QActionGroup(getBlock(parent))
                                   widget$setExclusive(TRUE)
                                   
                                   initFields(block=widget)
                                   set_items(items)
                                  # add_handler_changed(handler, action)

                                 },
                                 get_items=function(...) {
                                   items
                                 },
                                 clear_items=function() {
                                   sapply(widget$actions(), function(a) widget$removeAction(a))
                                 },
                                 make_items=function(menubar) {
                                   sapply(items, function(i) {
                                     a <- Qt$QAction(i, menubar)
                                     a$setCheckable(TRUE)
                                     widget$addAction(a)
                                     menubar$addAction(a)
                                   })
                                 },
                                 set_items=function(items,  ...) {
                                   clear_items()
                                   items <<- items
                                 },
                                 get_value=function(...) {
                                   items[get_index()]
                                 },
                                 set_value=function(value, ...) {
                                   idx <- match(value, items)
                                   if(!is.na(idx))
                                     set_index(idx)
                                 },
                                 get_index=function(...) {
                                   which(sapply(widget$actions(), "qinvoke", "isChecked"))
                                 },
                                 set_index=function(value, ...) {
                                   sapply(widget$actions(), "qinvoke", "setChecked", FALSE)
                                   widget$actions()[[value]]$setChecked(TRUE)
                                 },
                                 add_handler_changed=function(handler, action=NULL, ...) {
                                   add_handler("triggered", handler, action, ...)
                                 },
                                 set_exclusive=function(value) {
                                   "TRUE for radio buttons, FALSE for checkboxgroup"
                                   widget$setExclusive(as.logical(value))
                                 }
                                 ))
