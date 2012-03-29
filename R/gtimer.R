##' @include GWidget.R
NULL

##' S3 method for gtimer
##'
##' @inheritParams gWidgets2::gtimer
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gtimer guiWidgetsToolkitQt
##' @S3method .gtimer guiWidgetsToolkitQt
.gtimer.guiWidgetsToolkitQt <- function(toolkit, ms, FUN, data=NULL, one.shot=FALSE, start=TRUE)
  GTimer$new(toolkit, ms, FUN, data=data, one.shot=one.shot, start=start)

##' Timer class for gWidgets.
##'
##' The main reference methods \code{GTimer} are \code{start_timer} and \code{stop_timer}
##' @rdname gWidgets2Qt-package
GTimer <- setRefClass("GTimer",
                      fields=list(
                        timer="ANY",
                        interval="integer",
                        data="ANY",
                        FUN="ANY",
                        ID = "ANY"
                        ),
                      methods=list(
                        initialize=function(toolkit=guiToolkit(), ms, FUN=function(...) {},
                          data=NULL,
                          one.shot=FALSE, start=TRUE) {
                          


                          initFields(timer=Qt$QTimer(),
                                     data=data,
                                     FUN=FUN
                                     )

                          set_interval(ms)
                          timer$setSingleShot(one.shot)

                          qconnect(timer, "timeout", function() {
                            if(!is.null(data))
                              .self$FUN(data)
                            else
                              .self$FUN()
                          })

                          
                          if(start) 
                            start_timer()
                          
                          callSuper()
                        },
                        ## Main interface for gtimer:
                        set_interval=function(ms) {
                          "Set the interval. Need to stop and start active timer to implement."
                          interval <<- as.integer(ms)
                        },
                        start_timer = function() {
                          "Start the timer"
                          timer$start(interval)     

                        },
                        stop_timer = function() {
                          "stop the timer"
                          timer$stop()
                        }))
