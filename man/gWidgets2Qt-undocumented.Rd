\name{.addStockIcons.guiWidgetsToolkitQt}
\alias{.addStockIcons.guiWidgetsToolkitQt}
\alias{.gaction.guiWidgetsToolkitQt}
\alias{.galert.guiWidgetsToolkitQt}
\alias{.gbasicdialog.guiWidgetsToolkitQt}
\alias{.gbutton.guiWidgetsToolkitQt}
\alias{.gcalendar.guiWidgetsToolkitQt}
\alias{.gcheckbox.guiWidgetsToolkitQt}
\alias{.gcheckboxgroup.guiWidgetsToolkitQt}
\alias{.gcombobox.guiWidgetsToolkitQt}
\alias{.gconfirm.guiWidgetsToolkitQt}
\alias{.gdf.guiWidgetsToolkitQt}
\alias{.gedit.guiWidgetsToolkitQt}
\alias{.getStockIconByName.guiWidgetsToolkitQt}
\alias{.getStockIcons.guiWidgetsToolkitQt}
\alias{.gexpandgroup.guiWidgetsToolkitQt}
\alias{.gfile.guiWidgetsToolkitQt}
\alias{.gfilebrowse.guiWidgetsToolkitQt}
\alias{.gformlayout.guiWidgetsToolkitQt}
\alias{.gframe.guiWidgetsToolkitQt}
\alias{.ggraphics.guiWidgetsToolkitQt}
\alias{.ghtml.guiWidgetsToolkitQt}
\alias{.gimage.guiWidgetsToolkitQt}
\alias{.ginput.guiWidgetsToolkitQt}
\alias{.glabel.guiWidgetsToolkitQt}
\alias{.glayout.guiWidgetsToolkitQt}
\alias{.gmenu.guiWidgetsToolkitQt}
\alias{.gmessage.guiWidgetsToolkitQt}
\alias{.gnotebook.guiWidgetsToolkitQt}
\alias{.gpanedgroup.guiWidgetsToolkitQt}
\alias{.gradio.guiWidgetsToolkitQt}
\alias{.gseparator.guiWidgetsToolkitQt}
\alias{.gslider.guiWidgetsToolkitQt}
\alias{.gspinbutton.guiWidgetsToolkitQt}
\alias{.gstackwidget.guiWidgetsToolkitQt}
\alias{.gstatusbar.guiWidgetsToolkitQt}
\alias{.gtable.guiWidgetsToolkitQt}
\alias{.gtext.guiWidgetsToolkitQt}
\alias{.gtimer.guiWidgetsToolkitQt}
\alias{.gtoolbar.guiWidgetsToolkitQt}
\alias{.gtree.guiWidgetsToolkitQt}
\alias{.gvarbrowser.guiWidgetsToolkitQt}
\alias{.gwindow.guiWidgetsToolkitQt}
\alias{.stockIconFromObject.guiWidgetsToolkitQt}
\alias{addToStockIcons}
\title{add stock icons}
\usage{
  \method{.addStockIcons}{guiWidgetsToolkitQt}(toolkit,
    iconNames, iconFiles, ...)

  \method{.getStockIcons}{guiWidgetsToolkitQt}(toolkit,
    ...)

  \method{.getStockIconByName}{guiWidgetsToolkitQt}(toolkit,
    name, ...)

  addToStockIcons(iconNames, iconFiles)

  \method{.stockIconFromObject}{guiWidgetsToolkitQt}(toolkit,
    obj, ...)

  \method{.gwindow}{guiWidgetsToolkitQt}(toolkit, title,
    visible, name, width, height, parent, handler, action,
    ...)

  \method{.gaction}{guiWidgetsToolkitQt}(toolkit, label,
    tooltip = NULL, icon = NULL, key.accel = NULL,
    handler = NULL, action = NULL, parent = NULL, ...)

  \method{.gcalendar}{guiWidgetsToolkitQt}(toolkit,
    text = "", format = "\%Y-\%m-\%d", handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gcheckbox}{guiWidgetsToolkitQt}(toolkit, text,
    checked = FALSE, use.togglebutton = FALSE,
    handler = NULL, action = NULL, container = NULL, ...,
    parent = NULL)

  \method{.gcheckboxgroup}{guiWidgetsToolkitQt}(toolkit = NULL,
    items, checked = FALSE, horizontal = FALSE,
    use.table = FALSE, handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.gcombobox}{guiWidgetsToolkitQt}(toolkit, items,
    selected = 1, editable = FALSE, coerce.with = NULL,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gtable}{guiWidgetsToolkitQt}(toolkit, items,
    multiple = FALSE, chosen.col = 1, icon.col = NULL,
    tooltip.col = NULL, handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.gmenu}{guiWidgetsToolkitQt}(toolkit,
    menu.list = list(), popup = FALSE, container = NULL,
    ...)

  \method{.gmessage}{guiWidgetsToolkitQt}(toolkit, msg,
    title = "message",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.gconfirm}{guiWidgetsToolkitQt}(toolkit, msg,
    title = "Confirm",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.ginput}{guiWidgetsToolkitQt}(toolkit, msg,
    text = "", title = "Input",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.gbasicdialog}{guiWidgetsToolkitQt}(toolkit,
    title = "Dialog", parent = NULL, do.buttons = TRUE,
    handler = NULL, action = NULL, ...)

  \method{.galert}{guiWidgetsToolkitQt}(toolkit, msg,
    title = "message", delay = 3, parent = NULL, ...)

  \method{.gdf}{guiWidgetsToolkitQt}(toolkit, items = NULL,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gedit}{guiWidgetsToolkitQt}(toolkit, text = "",
    width = 25, coerce.with = NULL,
    initial.msg = initial.msg, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gfile}{guiWidgetsToolkitQt}(toolkit, text = "",
    type = c("open", "save", "selectdir"),
    initial.filename = NULL,
    filter = list(`All files` = list(patterns = c("*")), `R files` = list(patterns = c("*.R", "*.Rdata")), `text files` = list(mime.types = c("text/plain"))),
    multi = FALSE, ...)

  \method{.gfilebrowse}{guiWidgetsToolkitQt}(toolkit,
    text = "", type = c("open", "save", "selectdir"),
    initial.filename = NULL, filter = list(), quote = TRUE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gformlayout}{guiWidgetsToolkitQt}(toolkit,
    align = "left", spacing = 5, container = NULL, ...)

  \method{.ggraphics}{guiWidgetsToolkitQt}(toolkit,
    width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.ghtml}{guiWidgetsToolkitQt}(toolkit, x,
    container, ...)

  \method{.gimage}{guiWidgetsToolkitQt}(toolkit,
    filename = "", dirname = "", stock.id = NULL,
    size = "", handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.glayout}{guiWidgetsToolkitQt}(toolkit,
    homogeneous = FALSE, spacing = 10, container = NULL,
    ...)

  \method{.gnotebook}{guiWidgetsToolkitQt}(toolkit,
    tab.pos = 3, container = NULL, ...)

  \method{.gpanedgroup}{guiWidgetsToolkitQt}(toolkit,
    horizontal = TRUE, container = NULL, ...)

  \method{.gradio}{guiWidgetsToolkitQt}(toolkit, items,
    selected = 1, horizontal = FALSE, handler = NULL,
    action = NULL, container = NULL, ..., parent = NULL)

  \method{.gseparator}{guiWidgetsToolkitQt}(toolkit,
    horizontal = TRUE, container = NULL, ...)

  \method{.gslider}{guiWidgetsToolkitQt}(toolkit, from = 0,
    to = 100, by = 1, value = from, horizontal = TRUE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gspinbutton}{guiWidgetsToolkitQt}(toolkit,
    from = 0, to = 10, by = 1, value = from, digits = 0,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gstackwidget}{guiWidgetsToolkitQt}(toolkit,
    container = NULL, ...)

  \method{.gstatusbar}{guiWidgetsToolkitQt}(toolkit,
    text = "", container = NULL, ...)

  \method{.gtext}{guiWidgetsToolkitQt}(toolkit,
    text = NULL, width = NULL, height = 300,
    font.attr = NULL, wrap = TRUE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gtimer}{guiWidgetsToolkitQt}(toolkit, ms, FUN,
    data = NULL, one.shot = FALSE, start = TRUE)

  \method{.gtoolbar}{guiWidgetsToolkitQt}(toolkit,
    toolbar.list = list(),
    style = c("both", "icons", "text", "both-horiz"),
    container = NULL, ...)

  \method{.gtree}{guiWidgetsToolkitQt}(toolkit,
    offspring = NULL, offspring.data = NULL,
    chosen.col = 1, offspring.col = 2, icon.col = NULL,
    tooltip.col = NULL, multiple = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gvarbrowser}{guiWidgetsToolkitQt}(toolkit,
    handler = NULL, action = "summary", container = NULL,
    ...)

  \method{.gbutton}{guiWidgetsToolkitQt}(toolkit, text,
    handler, action, container, ...)

  \method{.glabel}{guiWidgetsToolkitQt}(toolkit, text = "",
    markup = FALSE, editable = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gframe}{guiWidgetsToolkitQt}(toolkit, text,
    markup, pos, horizontal = TRUE, spacing = 5,
    container = NULL, ...)

  \method{.gexpandgroup}{guiWidgetsToolkitQt}(toolkit,
    text, markup, horizontal = TRUE, handler = NULL,
    action = NULL, container = NULL, ...)
}
\arguments{
  \item{name}{name of icon}

  \item{obj}{R object to get icon from}

  \item{delay}{delay}

  \item{initial.msg}{If this is given and \code{text} is
  not, then an initial message is written to prompt the
  user.}

  \item{width}{width of device (pixels)}

  \item{height}{height of device (pixels)}

  \item{dpi}{dots per inch}

  \item{ps}{pointsize}

  \item{digits}{digits}

  \item{toolkit}{used to dispatch into toolkit if a
  separate implementation is made}

  \item{iconNames}{names of icons}

  \item{iconFiles}{path of icons}

  \item{...}{ignored}

  \item{title}{title for window's title bar. This is the
  main property and is accessed via \code{svalue} or
  \code{svalue<-}.}

  \item{visible}{logical. If code{TRUE} window is drawn
  when constructed. Otherwise, window can be drawn later
  using \code{visible<-}. This value can default to
  \code{FALSE} by setting the option:
  \code{options("gWidgets:gwindow-default-visible-is-false"=TRUE)}.
  There are advantages: windows can draw slowly when adding
  many items. With \pkg{gWidgets2RGtk2}, the
  \code{ggraphics} widget can like to be added to an
  undrawn widget as this avoids sizing issue.}

  \item{parent}{If non-NULL, can be used to suggest default
  location of window. The argument name was changed from
  location to parent. This can be a coordinate pair (x,y)
  with (0,0) the upper left corner, or a gwindow instance.
  In the latter case the location is suggested by the
  location of the current window. This is useful for
  placing dialogs near the parent window.}

  \item{handler}{handler for destroy event}

  \item{action}{action passed t handler}

  \item{label}{label for action}

  \item{tooltip}{toolktip for actin}

  \item{icon}{icon (stock icon name) for icon}

  \item{key.accel}{keyboard accelerator. If given, parent
  must be specified.}

  \item{text}{initial text}

  \item{format}{Date format}

  \item{container}{parent container}

  \item{checked}{is button selected}

  \item{use.togglebutton}{Use a toggle button (shows
  depressed) not a check box}

  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}

  \item{items}{checkbox labels}

  \item{horizontal}{logical. If true displayed
  horizontally, else vertically}

  \item{use.table}{logical. If supported, and \code{TRUE}
  then uses a table widget with scrollbars}

  \item{selected}{integer. Which item (by index) is
  selected. Use -1 for no selection}

  \item{editable}{logical. Is user allowed to edit value}

  \item{coerce.with}{A function of function name to be
  called before selected value is returned by
  \code{svalue}}

  \item{multiple}{logical allow multiple selectino}

  \item{chosen.col}{which value from the row is returned by
  selection}

  \item{icon.col}{NULL or integer. If latter, specifies
  column containing stock icon}

  \item{tooltip.col}{NULL or integer. If latter, specifies
  column containing tooltip}

  \item{x}{\code{GTable} object}

  \item{menu.list}{A list defining the menu structure.
  Named sub lists determine the submenu titles and
  structure. The list may have components of class:
  \code{GAction}, mapped to a button; \code{GSeparator},
  mapped to a horizontal separator; \code{GRadio}, mapped
  to linked buttons; or \code{GCheckbox}, mapped to a
  checkbox button.}

  \item{popup}{logical. If true, make a popup window to be
  added through a handler call}

  \item{msg}{Character. Message to display.}

  \item{do.buttons}{FALSE to suppress buttons when no
  parent}

  \item{type}{type of browser: to open a file, to save a
  file or to select a directory}

  \item{initial.filename}{Suggested file name}

  \item{filter}{A filter specifiation. This is toolkit
  specific. Here are some examples: \itemize{ \item{RGtk2}{
  Something like \preformatted{ list("All files" =
  list(patterns = c("*")), "R files" = list(patterns =
  c("*.R", "*.Rdata")), "text files" = list(mime.types =
  c("text/plain")) ) } } \item{tcltk}{} \item{Qt}{} }}

  \item{multi}{Logical. Allow multiple files to be
  selected?}

  \item{quote}{quote output}

  \item{align}{alignment of label. Left justify or center
  balance. Leave as "default" for underlying toolkit
  default.}

  \item{spacing}{spacing between columns}

  \item{filename}{basename of file}

  \item{dirname}{dirname of file}

  \item{stock.id}{stock id of icon (if non NULL)}

  \item{size}{size of icon when a stock id (toolkit
  dependent)}

  \item{homogeneous}{are cells all the same size}

  \item{tab.pos}{integer. Position of tabs, 1 on bottom, 2
  left, 3 top, 4 right. (If supported)}

  \item{from}{If a number of length one then a starting
  point, in which case to, by are passed to \code{seq}.
  Otherwise a sequence of values for which
  sort(unique(from)) will order}

  \item{to}{ending point when from is starting point}

  \item{by}{step size if not specified by \code{from}}

  \item{markup}{logical. If toolkit supports markup, this
  indicates it will be used}

  \item{font.attr}{font attributes for text buffer. One can
  also specify font attributes for insertion. The font
  attributes are specified with a named vector or list,
  with names and values coming from: \describe{
  \item{weight}{ in c("light", "normal", "bold", "heavy")}
  \item{style}{inc("normal", "oblique", "italic")}
  \item{family}{in c("sans", "helvetica", "times",
  "monospace")} \item{size}{in c("xx-small", "x-small",
  "small", "medium", "large", "x-large", "xx-large")}
  \item{foreground}{a value in colors()}
  \item{background}{a value in colors()} }}

  \item{wrap}{logical do lines wrap}

  \item{ms}{interval in milliseconds}

  \item{FUN}{FUnction to call. Has one argument, data
  passed in}

  \item{data}{passed to function}

  \item{one.shot}{logical. If TRUE, called just once, else
  repeats}

  \item{start}{logical. If FALSE, started by
  \code{start_timer} OO method. (Call
  \code{obj$start_time()}).}

  \item{toolbar.list}{list. A one-level list of
  \code{gaction} items, \code{gseparator} items or possibly
  other widgets. In the latter cases the \code{container}
  argument is not specified prior. (XXX Need to work this
  out with gWidgetstcltk)}

  \item{style}{style for icon or text.}

  \item{offspring}{function. A function passed values
  \code{path} and \code{data}, the latter from
  \code{offspring.data}. The path is the current position
  of the parent item using the named keys from the chosen
  column.}

  \item{offspring.data}{Passed to second argument of
  \code{offspring} function. Used to parameterize a
  function call.}

  \item{offspring.col}{integer or column name. Points to
  column containing logical values indicating if a row has
  offspring.}

  \item{pos}{position of label: 0=left, 1=right, some
  toolkit allow values in between}
}
\value{
  NULL
}
\description{
  add stock icons

  Returns list of icons. Names are stock ids; value is
  related to an icon (an icon or name...)

  return stock id. With Qt use \code{as_qicon} to create
  icon

  helper function

  return stock id from object

  toolkit constructor for gwindow

  Toolkit constructor

  Toolkit constructor

  Toolkit XXX constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  toolkit implementation for gmessage

  toolkit implementation for gconfirm

  toolkit implmentation of ginput

  toolkit implementation

  toolkit implementation of galert

  Toolkit constructor

  Toolkit gedit constructor

  Toolkit implementation

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit button constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit XXX constructor

  Toolkit constructor

  Toolkit constructor

  toolkit implementation

  S3 method for gtimer

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit label constructor

  gframe constructor

  toolkit constructor
}
\seealso{
  The documentation for this is found at
  \code{\link{gbutton}}.
}

