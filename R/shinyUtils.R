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



#' @title Launch an event in a Shiny application. Use only when the application becomes
#' too complex for simple reactive values
#'
#' @param type type of event to launch
#' @param userInfo a ReactiveValues object containing all the information about the
#' application
#'
#' @description The function will create - or update if it already exists - a list called
#' event in the userInfo object. The list contains two elements: value which is just a
#' numerical counter automatically increased when an event is launched and on which
#' dependencies should be taken and type to describe the type of event launched
#'
#' @return
#' @export
#'
#' @examples
launchEvent <- function(type, userInfo) {
  event <- isolate(userInfo$event)
  if (is.null(event)) {
    event <- list()
    event$value <- 0
  }
  event$value <- event$value + 1
  event$type <- type
  userInfo$event <- event
}
