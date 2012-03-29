##' @include GComponent.R
NULL

##' base class for container objects
GContainer <- setRefClass("GContainer",
                            contains="GComponentObservable",
                            fields=list(
                              children="list"
                              ),
                          methods=list(
                            add_child = function(child, expand, fill, anchor, ...) {
                              "Add child to parent, do internal book keeping"
                              stop("add_child call from subclass")
                            },
                            child_bookkeeping=function(child) {
                              "Update parent property of child and children property of parent container"
                              print(list("bookkeeping", child))
                              if(is(child, "GComponent"))
                                child$set_parent(.self)
                              children <<- c(children, child)
                            },
                            set_child_fill=function(fill) {
                              "Fill can be NULL, TRUE, FALSE, '', 'both', 'x', 'y'..."
                              if(missing(fill) || is.null(fill)) return(NULL)
                              if(is.logical(fill))
                                return(ifelse(fill, "both", NULL))
                              if(is.character(fill))
                                return(fill)
                            },
                            set_size_policy=function(child, fill) {
                              "If fill is non NULL, set the size policy based on its value"
                              if(!is.null(set_child_fill(fill))) {
                                if(fill == "y")
                                  child$setSizePolicy(Qt$QSizePolicy$Fixed, # Preferred? MinimumExpanding?
                                                      Qt$QSizePolicy$Expanding)
                                else if(fill == "x")
                                  child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                                      Qt$QSizePolicy$Fixed)
                                else                  # default is fill = "both" when no anchor, but expand
                                  child$setSizePolicy(Qt$QSizePolicy$Expanding,
                                                      Qt$QSizePolicy$Expanding)
                              }
                            },
                            xy_to_align=function(anchor=NULL) {
                              "return alignment value from anchor argument in x-y coordinates"

                              if(is.null(anchor))
                                return(0L)
                              
                              halign <- list(Qt$Qt$AlignLeft, Qt$Qt$AlignHCenter, Qt$Qt$AlignRight)
                              valign <- list(Qt$Qt$AlignBottom, Qt$Qt$AlignVCenter, Qt$Qt$AlignTop)
                              align <- Qt$Qt$AlignCenter
                              if(!is.null(anchor) && is.numeric(anchor) && length(anchor) >= 2) {
                                align <- halign[[anchor[1] + 2]] | valign[[anchor[2] + 2]]
                              }
                              align
                            },
                            update_gui=function(child) {
                              ## update GUI geometry
                              getBlock(child)$update()
                              getBlock(child)$updateGeometry()
                              getBlock(child)$show()
                            },
                            get_items=function(i, j, ..., drop=TRUE) {
                              "Return ith child in list of children"
                              items <- children[i]
                              if(drop && length(items) == 1)
                                items[[1]]
                              else
                                items
                            },
                            remove_child = function(child) {
                              "remove child from layout"
                              children <<- Filter(function(x) !identical(x, child), children)
                              child$set_parent(NULL)
                              getBlock(child)$hide()
                              widget$removeWidget(getBlock(child))
                            }


                            
                            )
                          )

                              
