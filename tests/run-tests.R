library(testthat)
library(gWidgets2)
options(guiToolkit="Qt")

f <- list.files(system.file("tests", package="gWidgets2"), full=T)
f <- Filter(function(x) !grepl("README", x), f)
f <- Filter(function(x) !grepl("window.R", x), f) ## isExtant call

sapply(f, function(i) {
  message("testing ", i)
  source(i)
})
