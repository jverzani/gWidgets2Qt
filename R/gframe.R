##' @include ggroup.R
NULL

##' gframe constructor
##'
##' @inheritParams gWidgets2::gframe
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gframe guiWidgetsToolkitQt
##' @S3method .gframe guiWidgetsToolkitQt
.gframe.guiWidgetsToolkitQt <- function(toolkit, text, markup, pos, horizontal=TRUE, spacing=5,container=NULL, ...) {
  GFrame$new(toolkit, text, markup, pos, horizontal, spacing, container, ...)
}



GFrame <- setRefClass("GFrame",
                      contains="GBoxContainer",
                      methods=list(
                        initialize=function(toolkit=NULL,
                          text="", markup=FALSE, pos=0, horizontal=TRUE, spacing=5,
                          container=NULL,use.scrollwindow=FALSE, ...) {
                          
                          horizontal <<- horizontal

                          block <<- Qt$QGroupBox()
                          if(pos < 0.33)
                            block$setAlignment(Qt$Qt$AlignLeft)
                          else if(pos > .66)
                            block$setAlignment(Qt$Qt$AlignRight)
                          else
                            block$setAlignment(Qt$Qt$AlignHCenter)
                          
                          
                          if(use.scrollwindow) {
                            sw <- make_scroll_widget(horizontal)
                            lyt <- Qt$QHBoxLayout()
                            lyt$addWidget(sw)
                            block$setLayout(lyt)
                          } else {
                            make_widget(horizontal)
                            block$setLayout(widget)
                          }
                          set_value(spacing)
                          set_names(text)
                          
                          add_to_parent(container, .self, ...)
                          callSuper(toolkit)
                        },
                        get_names=function() {
                          block$title
                        },
                        set_names=function(value) {
                          block$setTitle(as.character(value))
                        }
                        ))

                              

