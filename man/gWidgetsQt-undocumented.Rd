\name{.ggroup.guiWidgetsToolkitQt}
\alias{.ggroup.guiWidgetsToolkitQt}
\title{toolkit constructor for ggroup}
\usage{
  \method{.ggroup}{guiWidgetsToolkitQt} (toolkit, horizontal = TRUE, spacing = 5, use.scrollwindow = FALSE, container = NULL, ...)
}
\arguments{
  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}

  \item{horizontal}{logical. If TRUE, left to right layout,
  otherwise top to bottom}

  \item{spacing}{spacing aroud widget}

  \item{use.scrollwindow}{logical. If TRUE, places box
  container in scrollwindow. In this case, a size should be
  set.}

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
}
\description{
  toolkit constructor for ggroup
}

