#' R + viz.js
#' @description Make diagrams in R using
#' \href{https://github.com/mdaines/viz.js}{viz.js} with infrastructure
#' provided by \href{http://www.htmlwidgets.org/}{htmlwidgets}.
#' @param diagram spec for a diagram as either text, filename string, or
#' file connection.
#' @param engine string for the Graphviz layout engine; can be
#' \code{dot} (default), \code{neato}, \code{circo}, or \code{twopi}. For more
#' information see \href{viz.js Usage}{https://github.com/mdaines/viz.js#usage}.
#' @param allow_subst a boolean that enables/disables subsitution functionality.
#' @param options parameters supplied to the htmlwidgets framework.
#' @param width an optional parameter for specifying the width of the resulting
#' graphic in pixels.
#' @param height an optional parameter for specifying the height of the
#' resulting graphic in pixels.
#' @return An object of class \code{htmlwidget} that will
#' intelligently print itself into HTML in a variety of contexts
#' including the R console, within R Markdown documents,
#' and within Shiny output bindings.
#' @importFrom rstudioapi isAvailable
#' @export
grViz <- function(diagram = "",
                  engine = "dot",
                  allow_subst = TRUE,
                  options = NULL,
                  width = NULL,
                  height = NULL){
  
  # Check for a connection or file
  if (inherits(diagram, "connection") || file.exists(diagram)){
    diagram <- readLines(diagram, warn = FALSE)
    diagram <- paste0(diagram, collapse = "\n")
  } else {
    # Check for vector with length > 1 and concatenate
    if (length(diagram) > 1){
      diagram <- paste0(diagram, collapse = "\n")
    }
  }
  
  if (allow_subst == TRUE){
    diagram <- replace_in_spec(diagram)
  }
  
  # Single quotes within a diagram spec are problematic
  # so try to replace with \"
  diagram <- gsub(x = diagram, "'", "\"")
  
  # Forward options using x
  x <- list(
    diagram = diagram
    , config = list(
      engine = engine
      , options = options
    )
  )
  
  # Only use the viewer for newer versions of RStudio
  viewer.suppress <- !rstudioapi::isAvailable("0.99.120")
  #andy suppress the viewer
  #viewer.suppress <- TRUE
  
  
  # Create widget
  htmlwidgets::createWidget(
    name = "grViz",
    x = x,
    width = width,
    height = height,
    package = "DiagrammeR",
    htmlwidgets::sizingPolicy(viewer.suppress = viewer.suppress)
  )
}