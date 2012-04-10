\name{.gtree.guiWidgetsToolkitRGtk2}
\alias{.gtree.guiWidgetsToolkitRGtk2}
\title{Toolkit constructor}
\usage{
  \method{.gtree}{guiWidgetsToolkitRGtk2} (toolkit,
    offspring = NULL, offspring.data = NULL,
    chosen.col = 1, offspring.col = 2, icon.col = NULL,
    tooltip.col = NULL, multiple = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)
}
\arguments{
  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}

  \item{offspring}{function. A function passed values
  \code{path} and \code{data}, the latter from
  \code{offspring.data}. The path is the current position
  of the parent item using the named keys from the chosen
  column.}

  \item{offspring.data}{Passed to second argument of
  \code{offspring} function. Used to parameterize a
  function call.}

  \item{chosen.col}{integer or one of column names of data
  frame returned by \code{offspring}. The chosen column
  gives the key and value of the path.}

  \item{offspring.col}{integer or column name. Points to
  column containing logical values indicating if a row has
  offspring.}

  \item{icon.col}{integer of one of the column names of the
  data frame. If provided (non-NULL), then this column
  should provide a stock icon name to be placed in the row
  for the given data.}

  \item{tooltip.col}{integer or one of the column names of
  the data frame. If provided (non-NULL), then the row for
  this item will have a tooltip given by the pointed to
  value.}

  \item{multiple}{logical. Is multiple selection allowed?}

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

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{...}{passed to update method}
}
\description{
  Toolkit constructor
}

