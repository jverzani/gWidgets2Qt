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
\alias{.gtimer.guiWidgetsToolkitQt}
\alias{.stockIconFromObject.guiWidgetsToolkitQt}
\alias{addToStockIcons}
\title{add stock icons}
\usage{
  \method{.addStockIcons}{guiWidgetsToolkitQt} (toolkit,
    iconNames, iconFiles, ...)

  \method{.getStockIcons}{guiWidgetsToolkitQt} (toolkit,
    ...)

  \method{.getStockIconByName}{guiWidgetsToolkitQt}
    (toolkit, name, ...)

  addToStockIcons(iconNames, iconFiles)

  \method{.stockIconFromObject}{guiWidgetsToolkitQt}
    (toolkit, obj, ...)

  .gbutton.guiWidgetsToolkitQt(toolkit, text, handler,
    action, container, ...)

  \method{.gframe}{guiWidgetsToolkitQt} (toolkit, text,
    markup, pos, horizontal = TRUE, spacing = 5,
    container = NULL, ...)

  .glabel.guiWidgetsToolkitQt(toolkit, text = "",
    markup = FALSE, editable = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gexpandgroup}{guiWidgetsToolkitQt} (toolkit,
    text, markup, horizontal = TRUE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.glayout}{guiWidgetsToolkitQt} (toolkit,
    homogeneous = FALSE, spacing = 10, container = NULL,
    ...)

  \method{.gpanedgroup}{guiWidgetsToolkitQt} (toolkit,
    horizontal = TRUE, container = NULL, ...)

  \method{.gnotebook}{guiWidgetsToolkitQt} (toolkit,
    tab.pos = 3, container = NULL, ...)

  \method{.gstackwidget}{guiWidgetsToolkitQt} (toolkit,
    container = NULL, ...)

  \method{.gformlayout}{guiWidgetsToolkitQt} (toolkit,
    align = "left", spacing = 5, container = NULL, ...)

  \method{.gcheckbox}{guiWidgetsToolkitQt} (toolkit, text,
    checked = FALSE, use.togglebutton = FALSE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gcheckboxgroup}{guiWidgetsToolkitQt}
    (toolkit = NULL, items, checked = FALSE,
    horizontal = FALSE, use.table = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gradio}{guiWidgetsToolkitQt} (toolkit, items,
    selected = 1, horizontal = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gslider}{guiWidgetsToolkitQt} (toolkit,
    from = 0, to = 100, by = 1, value = from,
    horizontal = TRUE, handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.gspinbutton}{guiWidgetsToolkitQt} (toolkit,
    from = 0, to = 10, by = 1, value = from, digits = 0,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gstatusbar}{guiWidgetsToolkitQt} (toolkit,
    text = "", container = NULL, ...)

  \method{.gseparator}{guiWidgetsToolkitQt} (toolkit,
    horizontal = TRUE, container = NULL, ...)

  \method{.gedit}{guiWidgetsToolkitQt} (toolkit, text = "",
    width = 25, coerce.with = NULL,
    initial.msg = initial.msg, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gaction}{guiWidgetsToolkitQt} (toolkit, label,
    tooltip = NULL, icon = NULL, key.accel = NULL,
    handler = NULL, action = NULL, parent = NULL, ...)

  \method{.gmessage}{guiWidgetsToolkitQt} (toolkit, msg,
    title = "message",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.gconfirm}{guiWidgetsToolkitQt} (toolkit, msg,
    title = "Confirm",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.ginput}{guiWidgetsToolkitQt} (toolkit, msg,
    text = "", title = "Input",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.gbasicdialog}{guiWidgetsToolkitQt} (toolkit,
    title = "Dialog", parent = NULL, do.buttons = TRUE,
    handler = NULL, action = NULL, ...)

  \method{.galert}{guiWidgetsToolkitQt} (toolkit, msg,
    title = "message", delay = 3, parent = NULL, ...)

  \method{.gfile}{guiWidgetsToolkitQt} (toolkit, text = "",
    type = c("open", "save", "selectdir"),
    initial.filename = NULL,
    filter = list(`All files` = list(patterns = c("*")), `R files` = list(patterns = c("*.R", "*.Rdata")), `text files` = list(mime.types = c("text/plain"))),
    multi = FALSE, ...)

  \method{.gfilebrowse}{guiWidgetsToolkitQt} (toolkit,
    text = "", type = c("open", "save", "selectdir"),
    initial.filename = NULL, filter = list(), quote = TRUE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.ggraphics}{guiWidgetsToolkitQt} (toolkit,
    width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gcalendar}{guiWidgetsToolkitQt} (toolkit,
    text = "", format = "\%Y-\%m-\%d", handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gtimer}{guiWidgetsToolkitQt} (toolkit, ms, FUN,
    data = NULL, one.shot = FALSE, start = TRUE)

  \method{.gcombobox}{guiWidgetsToolkitQt} (toolkit, items,
    selected = 1, editable = FALSE, coerce.with = NULL,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gimage}{guiWidgetsToolkitQt} (toolkit,
    filename = "", dirname = "", stock.id = NULL,
    size = "", handler = NULL, action = NULL,
    container = NULL, ...)

  .ghtml.guiWidgetsToolkitQt(toolkit, x, container, ...)

  \method{.gtable}{guiWidgetsToolkitQt} (toolkit, items,
    multiple = FALSE, chosen.col = 1, icon.col = NULL,
    tooltip.col = NULL, handler = NULL, action = NULL,
    container = NULL, ...)
}
\arguments{
  \item{name}{name of icon}

  \item{obj}{R object to get icon from}

  \item{digits}{digits}

  \item{initial.msg}{If this is given and \code{text} is
  not, then an initial message is written to prompt the
  user.}

  \item{delay}{delay}

  \item{width}{width of device (pixels)}

  \item{height}{height of device (pixels)}

  \item{dpi}{dots per inch}

  \item{ps}{pointsize}

  \item{toolkit}{used to dispatch into toolkit if a
  separate implementation is made}

  \item{iconNames}{names of icons}

  \item{iconFiles}{path of icons}

  \item{...}{ignored}

  \item{text}{frame label}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{markup}{does label use markup (toolkit specific)}

  \item{pos}{position of label: 0=left, 1=right, some
  toolkit allow values in between}

  \item{horizontal}{logical. If TRUE, left to right layout,
  otherwise top to bottom}

  \item{spacing}{spacing aroud widget}

  \item{homogeneous}{are cells all the same size}

  \item{handler}{A handler assigned to the default change
  signal. Handlers are called when some event triggers a
  widget to emit a signal. For each widget some default
  signal is assumed, and handlers may be assigned to that
  through \code{addHandlerChanged} or at construction time.
  Handlers are functions whose first argument, \code{h} in
  the documentation, is a list with atleast two components
  \code{obj}, referring to the object emitting the signal
  and \code{action}, which passes in user-specified data to
  parameterize the function call.}

  \item{action}{User supplied data passed to the handler
  when it is called}

  \item{tab.pos}{integer. Position of tabs, 1 on bottom, 2
  left, 3 top, 4 right. (If supported)}

  \item{align}{alignment of label. Left justify or center
  balance. Leave as "default" for underlying toolkit
  default.}

  \item{checked}{is button selected}

  \item{use.togglebutton}{Use a toggle button (shows
  depressed) not a check box}

  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}

  \item{items}{checkbox labels}

  \item{use.table}{logical. If supported, and \code{TRUE}
  then uses a table widget with scrollbars}

  \item{selected}{index of initially selected item}

  \item{from}{If a number of length one then a starting
  point, in which case to, by are passed to \code{seq}.
  Otherwise a sequence of values for which
  sort(unique(from)) will order}

  \item{to}{ending point when from is starting point}

  \item{by}{step size if not specified by \code{from}}

  \item{editable}{If TRUE, then clicking on label will
  enable editing of value}

  \item{label}{label for action}

  \item{tooltip}{toolktip for actin}

  \item{icon}{icon (stock icon name) for icon}

  \item{key.accel}{keyboard accelerator. If given, parent
  must be specified.}

  \item{parent}{parent window. Needed if keyboard
  accelerator used.}

  \item{msg}{Character. Message to display.}

  \item{title}{Character. Title of window}

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

  \item{format}{Date format}

  \item{ms}{interval in milliseconds}

  \item{FUN}{FUnction to call. Has one argument, data
  passed in}

  \item{data}{passed to function}

  \item{one.shot}{logical. If TRUE, called just once, else
  repeats}

  \item{start}{logical. If FALSE, started by
  \code{start_timer} OO method. (Call
  \code{obj$start_time()}).}

  \item{coerce.with}{A function of function name to be
  called before selected value is returned by
  \code{svalue}}

  \item{filename}{basename of file}

  \item{dirname}{dirname of file}

  \item{stock.id}{stock id of icon (if non NULL)}

  \item{size}{size of icon when a stock id (toolkit
  dependent)}

  \item{multiple}{logical allow multiple selectino}

  \item{chosen.col}{which value from the row is returned by
  selection}

  \item{icon.col}{NULL or integer. If latter, specifies
  column containing stock icon}

  \item{tooltip.col}{NULL or integer. If latter, specifies
  column containing tooltip}
}
\value{
  NULL
}
\description{
  add stock icons

  Returns list of icons. Names are stock ids; value is
  related to an icon (an icon or name...)

  return stock id

  helper function

  return stock id from object

  Toolkit button constructor

  gframe constructor

  Toolkit label constructor

  toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit XXX constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit XXX constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit gedit constructor

  Toolkit constructor

  toolkit implementation for gmessage

  toolkit implementation for gconfirm

  toolkit implmentation of ginput

  toolkit implementation

  toolkit implementation of galert

  Toolkit implementation

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  S3 method for gtimer

  Toolkit constructor

  Toolkit constructor

  Toolkit button constructor

  Toolkit constructor
}

