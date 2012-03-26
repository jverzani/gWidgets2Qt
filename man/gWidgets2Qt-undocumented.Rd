\name{.gbutton.guiWidgetsToolkitQt}
\alias{.gbutton.guiWidgetsToolkitQt}
\alias{.gcheckbox.guiWidgetsToolkitQt}
\alias{.gcheckboxgroup.guiWidgetsToolkitQt}
\alias{.gexpandgroup.guiWidgetsToolkitQt}
\alias{.gformlayout.guiWidgetsToolkitQt}
\alias{.gframe.guiWidgetsToolkitQt}
\alias{.glabel.guiWidgetsToolkitQt}
\alias{.glayout.guiWidgetsToolkitQt}
\alias{.gnotebook.guiWidgetsToolkitQt}
\alias{.gpanedgroup.guiWidgetsToolkitQt}
\alias{.gradio.guiWidgetsToolkitQt}
\alias{.gslider.guiWidgetsToolkitQt}
\alias{.gstackwidget.guiWidgetsToolkitQt}
\title{Toolkit button constructor}
\usage{
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
}
\arguments{
  \item{toolkit}{toolkit}

  \item{text}{frame label}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations. For example, when using a widget as a
  menubar object one can specify a parent argument to pass
  in parent information, similar to how the argument is
  used with gaction and the dialogs.}

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
  balance.}

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
}
\description{
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
}

