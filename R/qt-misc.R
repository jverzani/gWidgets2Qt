##' @include misc.R
NULL

## import getWidget, getBlock for gWidgets2 for this, we don't need to export. This is used internally only

##' method for stopping getWidget
##' 
##' @param obj an RQtObject
##' @export
##' @method getWidget RQtObject
##' @S3method getWidget RQtObject
getWidget.RQtObject <- function(obj) obj

##' S3 method for stopping getBlock
##'
##' @param obj an RQtObject
##' @export
##' @method getBlock RQtObject
##' @S3method getBlock RQtObject
getBlock.RQtObject <- function(obj) obj




##################################################
## Fonts
makeQFont <- function(font.attr) {
  f <- Qt$QFont()

  ## process into a list
  if(is.list(font.attr))
    l <- font.attr
  else
    l <- lapply(font.attr, function(i) i)

  ## family
  if(!is.null(l$family)) {
    family <- switch(l$family,
                     "normal"="arial",
                     "sans" = "arial",
                     "serif" = "times",
                     "monospace" = "courier",
                     l$family)
    f$setFamily(family)
  }
  ## sizse
  if(!is.null(l$size)) {
    if(is.numeric(l$size)) {
      val <- as.integer(l$size)
    } else {
      val <- switch(l$size,
                    "xx-large" = 24L,
                    "x-large" = 20L,
                    "large" = 16L,
                    "medium" = 14L,
                    "small" = 12L,
                    "x-small" = 10L,
                    "xx-small" = 8L,
                    12L
                    )
    }
    f$setPixelSize(val)
      
  }
  ## style
  if(!is.null(l$style)) {
    QtStyles <- c("normal"=Qt$QFont$StyleNormal,
                  "italic"=Qt$QFont$StyleItalic,
                  "oblique"=Qt$QFont$StyleOblique)
    if(l$style %in% names(QtStyles))
      f$setStyle(QtStyles[l$style])
  }
  ## weight
  if(!is.null(l$weight)) {
    QtWeights = c(
      "ultra-light"=0L,
      "light" = Qt$QFont$Light,
      "normal"=Qt$QFont$Normal,
      "bold"=Qt$QFont$Bold,
      "ultra-bold" = Qt$QFont$Black,
      "heavy" = 99L)
    if(l$weight == "bold")
      f$setBold(TRUE)
    else if(l$weight %in% QtWeights)
      f$setWeight(QtWeights[l$weight])
  }
  return(f)
}

makeQTextCharFormat<- function(font.attr) {
  tcf <- Qt$QTextCharFormat()
  tcf$setFont(makeQFont(font.attr))
  tcf
}
