##' @include GWidget.R
NULL


##' Toolkit button constructor
##'
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .ghtml guiWidgetsToolkitQt
##' @S3method .ghtml guiWidgetsToolkitQt
.ghtml.guiWidgetsToolkitQt <- function(toolkit, x, container, ...) {
    GHtml$new(toolkit, x, container, ...)
}

##' Button reference class
GHtml <- setRefClass("GHtml",
                     contains="GWidget",
                     fields=list(
                       url="character"
                       ),
                     methods=list(
                       initialize=function(toolkit, x, container, ...) {

                         widget <<- Qt$QWebView()
                         initFields(block=widget)

                         set_value(x)

                         add_to_parent(container, .self, ...)
                         callSuper(toolkit)
                       },
                       is_url=function(x) {
                         grepl("^(ftp|http|file)://", x)
                       },
                       set_value=function(value, drop=TRUE, ...) {
                         if(missing(value))
                           return()

                         url <<- value
                         
                         if(is_url(value))
                           widget$setUrl(Qt$QUrl(value))
                         else
                           widget$setHtml(value)
                       },
                       get_value=function(index=TRUE, drop=TRUE, ...) {
                         url            # possibly HTML fragment
                       }
                       ))
