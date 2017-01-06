library(shiny)

#' textOutput2
#'
#' Alternative to shiny textOutput to allow multiple classes on the
#' text element and to allow the user of returning text directly
#' instead of having to wrap it in a renderText element before
#'
#' @param id
#' @param content
#' @param container
#' @param inline
#' @param class
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
textOutput2 <- function(id, content = NULL, container = if (inline) htmltools::span else htmltools::div,
                         inline = FALSE, class="", ...) {
  classes <- paste(class, "shiny-text-output")
  if (is.null(content))
  {
    container(id = id, class = classes, ...)
  } else {
    container(content, class = classes, ...)
  }
}

