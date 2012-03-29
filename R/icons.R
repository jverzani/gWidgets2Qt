##' @include misc.R
NULL



##' add stock icons
##'
##' @export
##' @inheritParams gWidgets2::addStockIcons
##' @rdname gWidgets2Qt-undocumented
##' @method .addStockIcons guiWidgetsToolkitQt
##' @S3method .addStockIcons guiWidgetsToolkitQt
.addStockIcons.guiWidgetsToolkitQt <- function(toolkit, iconNames, iconFiles,... ) {
  .GWidgetsQtIcons$add_to_gtk_stock_icons(iconNames, iconFiles)
}

##' Returns list of stock ids
##'
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .getStockIcons guiWidgetsToolkitQt
##' @S3method .getStockIcons guiWidgetsToolkitQt
.getStockIcons.guiWidgetsToolkitQt <- function(toolkit, ...) {
  lst <- .GWidgetsQtIcons$icons
  as.list(lst)
}

##' return stock id
##'
##' @param name name of icon
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .getStockIconByName guiWidgetsToolkitQt
##' @S3method .getStockIconByName guiWidgetsToolkitQt
.getStockIconByName.guiWidgetsToolkitQt <- function(toolkit, name, ...) {
  icons <- getStockIcons(toolkit)

  if(!missing(name) && is.character(name) && nchar(name) > 0)
    icons[[name, exact=TRUE]]
  else
    NULL
}


##' helper function
##'
##' @export
##' @rdname gWidgets2Qt-undocumented
addToStockIcons <- function(iconNames, iconFiles) {
  .GWidgetsQtIcons$add_to_stock_icons(iconNames, iconFiles)
}

## class to hold icons
## main propoerty is icons a list holding the icons by name
GWidgetsQtIcons <- setRefClass("GWidgetsQtIcons",
                                  contains="GWidgets2Icons",
                                  methods=list(
                                    update_icons=function() {
                                      ## make gWidgets Icons in icons
                                      callSuper()
                                      ## makes icons in icons with files
                                      add_to_qt_stock_icons(names(icons), icons)
                                      ## add in stock icons, will overwrite
                                      ## List of stock icons from style
qt_stock_icons <- list(
              min=Qt$QStyle$SP_TitleBarMinButton,
              menu=Qt$QStyle$SP_TitleBarMenuButton,
              max=Qt$QStyle$SP_TitleBarMaxButton,
              close=Qt$QStyle$SP_TitleBarCloseButton,
              normal=Qt$QStyle$SP_TitleBarNormalButton,
              shade=Qt$QStyle$SP_TitleBarShadeButton,
              unshade=Qt$QStyle$SP_TitleBarUnshadeButton	,
              help=Qt$QStyle$SP_TitleBarContextHelpButton	,
              info=Qt$QStyle$SP_MessageBoxInformation	,
              warning=Qt$QStyle$SP_MessageBoxWarning	,
              critical=Qt$QStyle$SP_MessageBoxCritical	,
              question=Qt$QStyle$SP_MessageBoxQuestion	,
              desktop=Qt$QStyle$SP_DesktopIcon	,
              trash=Qt$QStyle$SP_TrashIcon	,
              computer=Qt$QStyle$SP_ComputerIcon	,
              drive_fdi=Qt$QStyle$SP_DriveFDIcon	,
              drive_hdi=Qt$QStyle$SP_DriveHDIcon	,
              drive_cd=Qt$QStyle$SP_DriveCDIcon	,
              drive_dvd=Qt$QStyle$SP_DriveDVDIcon	,
              drive_net=Qt$QStyle$SP_DriveNetIcon	,
              home=Qt$QStyle$SP_DirHomeIcon	,
              open=Qt$QStyle$SP_DirOpenIcon	,
              closed=Qt$QStyle$SP_DirClosedIcon	,
              icon=Qt$QStyle$SP_DirIcon	,
              link=Qt$QStyle$SP_DirLinkIcon	,
              file=Qt$QStyle$SP_FileIcon	,
              file_link=Qt$QStyle$SP_FileLinkIcon	,
              start=Qt$QStyle$SP_FileDialogStart	,
              end=Qt$QStyle$SP_FileDialogEnd	,
              to_parent=Qt$QStyle$SP_FileDialogToParent	,
              new_folder=Qt$QStyle$SP_FileDialogNewFolder,
              detail=Qt$QStyle$SP_FileDialogDetailedView	,
              file_info=Qt$QStyle$SP_FileDialogInfoView	,
              contents=Qt$QStyle$SP_FileDialogContentsView	,
              file_list=Qt$QStyle$SP_FileDialogListView	,
              back=Qt$QStyle$SP_FileDialogBack	,
              close=Qt$QStyle$SP_DockWidgetCloseButton,
              horizontal_extension=Qt$QStyle$SP_ToolBarHorizontalExtensionButton	,
              vertical_extension=Qt$QStyle$SP_ToolBarVerticalExtensionButton	,
              ok=Qt$QStyle$SP_DialogOkButton	,
              cancel=Qt$QStyle$SP_DialogCancelButton	,
              help=Qt$QStyle$SP_DialogHelpButton	,
              open=Qt$QStyle$SP_DialogOpenButton	,
              save=Qt$QStyle$SP_DialogSaveButton	,
              close=Qt$QStyle$SP_DialogCloseButton	,
              apply=Qt$QStyle$SP_DialogApplyButton	,
              reset=Qt$QStyle$SP_DialogResetButton	,
              discard=Qt$QStyle$SP_DialogDiscardButton,
              yes=Qt$QStyle$SP_DialogYesButton	,
              no=Qt$QStyle$SP_DialogNoButton	,
              arrow_up=Qt$QStyle$SP_ArrowUp	,
              arrow_down=Qt$QStyle$SP_ArrowDown	,
              arrow_left=Qt$QStyle$SP_ArrowLeft	,
              arrow_right=Qt$QStyle$SP_ArrowRight	,
              arrow_back=Qt$QStyle$SP_ArrowBack	,
              arrow_forward=Qt$QStyle$SP_ArrowForward,
              link=Qt$QStyle$SP_CommandLink	,
              shield=Qt$QStyle$SP_VistaShield	,
              reload=Qt$QStyle$SP_BrowserReload	,
              browser_stop=Qt$QStyle$SP_BrowserStop	,
              media_play=Qt$QStyle$SP_MediaPlay	,
              media_stop=Qt$QStyle$SP_MediaStop	,
              media_pause=Qt$QStyle$SP_MediaPause	,
              media_skip_forward=Qt$QStyle$SP_MediaSkipForward,
              media_skip_backward=Qt$QStyle$SP_MediaSkipBackward,
              media_seek_forward=Qt$QStyle$SP_MediaSeekForward	,
              media_seek_backward=Qt$QStyle$SP_MediaSeekBackward	,
              media_volume=Qt$QStyle$SP_MediaVolume	,
              media_mute=Qt$QStyle$SP_MediaVolumeMuted
              )

                                      mapply(function(name, icon) icons[[name]] <<- icon, names(qt_stock_icons), qt_stock_icons)
                                    },
                                    add_to_qt_stock_icons = function(iconNames, iconFiles) {
                                      f <- function(name, icon) {
                                        if(is(icon, "QIcon")) {
                                          icons[[name]] <<- icon
                                        } else if(is.character(icon) && file.exists(icon)) {
                                          icons[[name]] <<- Qt$QIcon(icon)
                                        } else {
                                          icons[[name]] <<- icon
                                        }
                                      }
                                      mapply(f, iconNames, unlist(iconFiles))
                                    },
                                    add_qt_stock_icons=function() {
                                      ## add qt__stock_icons
                                      
                                      sapply(seq_along(names(qt_stock_icons)), function(nm) {
                                        icons[[nm]] <<- style$standardIcon(qt_stock_icons[[nm, exact=TRUE]])
                                      })
                                    }
                                    
                                    
                                    ))

.GWidgetsQtIcons <- GWidgetsQtIcons$new()

load_gwidget_icons <- function() {
  ## add the icons
  ## we use xpm icons gimp can convert
  iconFileNames <- list.files(system.file("images", package="gWidgets2"), full.names=TRUE)
  iconFileNames <- Filter(function(x) grepl("\\.gif$", x), iconFileNames)
  iconNames <- basename(iconFileNames)
  iconNames <- gsub("\\.gif$","",iconNames)
  .GWidgetsQtIcons$add_to_qt_stock_icons(iconNames, iconFileNames)
}

##################################################

##' return stock id from object
##'
##' @param obj R object to get icon from
##' @export
##' @rdname gWidgets2Qt-undocumented
##' @method .stockIconFromObject guiWidgetsToolkitQt
##' @S3method .stockIconFromObject guiWidgetsToolkitQt
.stockIconFromObject.guiWidgetsToolkitQt <- function(toolkit, obj, ...) {
  icon_for_object <- function(x) UseMethod("icon_for_object")
  icon_for_object.default <- function(x) "gw-symbol_dot"
  icon_for_object.numeric <- function(x) "gtk-numeric"
  icon_for_object.numeric <- function(x) "gw-numeric"
  icon_for_object.factor <- function(x) "gw-factor"
  icon_for_object.character <- function(x) "gw-character"
  icon_for_object.function <- function(x) "fw-function"
  icon_for_object.data.frame <- function(x) "gw-dataframe"
  
  icon_for_object(obj)
}

