\name{createGQWClass}
\alias{createGQWClass}
\title{Create a Sub class of a QWidget class so that we can run virtual Events}
\usage{
  createGQWClass(cname, constructor)
}
\description{
  This is necessary for many widgets, though should be used
  sparingly as it will be much slower -- things are looked
  up in R, not C++ code These are used as follows: a) use
  \code{createGQWClass} to create the class b) instantiate
  c) call setObject to set reference class container.
}

