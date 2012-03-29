##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gcalendar
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .gcalendar guiWidgetsToolkitQt
##' @S3method .gcalendar guiWidgetsToolkitQt
.gcalendar.guiWidgetsToolkitQt <-  function(toolkit,
                                               text="",
                                               format="%Y-%m-%d",
                                               handler = NULL,action = NULL, container = NULL, ... ) {
  GCalendar$new(toolkit,
                 text=text,
                format=format,
                handler = handler,action = action, container = container, ...)
}

## put calendar into a dialog
qsetClass("DateDialog", Qt$QDialog, 
          function(parent = NULL, title="Choose a date") {
            super(parent=parent)
            setWindowTitle(gettext(title))
            this$calendar <- Qt$QCalendarWidget()

            ##
            btn_box <- 
              Qt$QDialogButtonBox(Qt$QMessageBox$Cancel | 
                                  Qt$QMessageBox$Ok)

            qconnect(btn_box, "accepted", function() {
              this$date <- calendar$selectedDate
              this$close()
              this$setResult(TRUE)      # 1
            })

            qconnect(calendar, "activated", function(date) {
              this$date <- calendar$selectedDate
              this$close()
              this$setResult(TRUE)    
            })
            qconnect(btn_box, "rejected", function() {
              this$date <- NULL
              this$close()
              this$setResult(FALSE)     # 0
            })

            ##
            layout <- Qt$QVBoxLayout()
            sapply(list(calendar, btn_box), layout$addWidget)
            setLayout(layout)
          })
qsetProperty("date", DateDialog)
qsetMethod("setSelectedDate", DateDialog, function(date) calendar$setSelectedDate(date))

## Calendar
GCalendar <- setRefClass("GCalendar",
                         contains="GWidget",
                         fields=list(
                           "format"="character"
                           ),
                         methods=list(
                           initialize=function(toolkit=NULL,
                             text="",
                             format="%Y-%m-%d",
                             handler, action, container, ...) {


                             
                             if(text == "" && format != "")
                               text = format(Sys.Date(), format)
                             text = as.character(text)

                             initFields(widget=Qt$QLineEdit(),
                                        block=Qt$QWidget(),
                                        format=format,
                                        change_signal="returnPressed"
                                        )
                             widget$setSizePolicy(Qt$QSizePolicy$Expanding, Qt$QSizePolicy$Fixed)
                                        

                             ## layout
                             btn <- Qt$QPushButton(gettext("date ..."))
                             lyt <- Qt$QHBoxLayout()
                             lyt$addWidget(widget, stretch=2)
                             lyt$addWidget(btn, stretch=0)
                             block$setLayout(lyt)


                             ## put cal into a dialog
                             cal <- DateDialog()



                             ## set date if possible
                             day <- try(as.Date(text), silent=TRUE)
                             if(!inherits(day, "try-error")) {
                               yr <- as.integer(format(day,"%Y"))
                               mo <- as.integer(format(day,"%m"))
                               dy <- as.integer(format(day,"%d"))
                               cal$setSelectedDate(Qt$QDate(yr, mo, dy))
                             }
                             
                             
                             ## add handler to btn
                             qconnect(btn, "clicked", function() {
                               out <- cal$exec()
                               if(out)
                                 set_value(format_qt_date(cal$date))
                             })
                            
                             
                             add_to_parent(container, .self, ...)
                             
                             handler_id <<- add_handler_changed(handler, action)
                             
                             callSuper(toolkit)
                           },
                           format_qt_date = function(date) {
                             if(is.null(date)) return("")
                             
                             yr <- date$year()
                             mo <- date$month()
                             dy <- date$day()
                             base:::format(paste(c(yr, mo, dy), collapse="-"), format=format)
                           },
                           get_value=function( ...) {
                             val <- widget$text
                             cur_date <- try(as.Date(val, format=format))
                             if(inherits(cur_date,"try-error"))
                               val <- NA
                             else
                               val <- as.character(cur_date)
                           },
                           set_value=function(value, ...) {
                             widget$setText(value)
                             invoke_change_handler()
                           }
                           ))

