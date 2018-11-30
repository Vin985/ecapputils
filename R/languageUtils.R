library(shiny)
library(stringr)


#' Title
#'
#' @param path.to.file The path to the language file. The file must be a semi-colon
#' separated file with one column containing the keys and the others containing the
#' languages. The first line should contain the title for each column
#' @param reload Reload language list if the language file has been modified since last
#' access
#'
#' @return a data.frame containing all the language keys
#' @export
#'
#' @examples
loadLanguages <- function(path.to.file, reload = TRUE) {
  return(list(
    data = read.csv2(
      path.to.file,
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    ),
    mtime = file.mtime(path.to.file),
    path = path.to.file,
    reload = reload
  ))
}




getLanguageData <- function(data = NULL) {
  ### retrieve language data
  d <- NULL
  # First use the provided language data if available
  if (!is.null(data)) {
    d <- data
    # else check if there is data in the global environment
  } else if (exists("LANG_DATA")) {
    d <- LANG_DATA
  } else {
    stop("A data.frame with all languages should be provided")
  }
  return(d)
}




getAllLanguages <- function(data = NULL) {
  d <- getLanguageData(data)$data
  return(names(d)[-1])
}




getAvailableLanguages <- function(lang, data = NULL) {
  d <- getLanguageData(data)$data
  langs <- names(d)[-1]
  return(langs[langs != lang])
}




#' Title
#'
#' @param key
#' @param lang
#' @param data
#'
#' @return
#' @export
#'
#' @examples
geti18nValue <- function(key, lang = NULL , data = NULL) {
  # Select language
  lg <- NULL

  # First check if a language is provided
  if (!is.null(lang)) {
    lg <- lang
    # Else check if a global variable LANG is set
  } else if (exists("LANG")) {
    lg <- LANG
  } else {
    stop("A language should be provided!")
  }

  ### retrieve language data
  d <- getLanguageData(data)

  # Check if there is a new version of the language file available
  if (!is.null(d$mtime) &&
      d$reload && file.mtime(d$path) != d$mtime) {
    print("reloading")
    d <- loadLanguages(d$path)
    LANG_DATA <<- d
  }

  dat <- d$data
  idx <- match(key, dat[, 1])

  # Keys should be in the first column
  return(ifelse(is.na(idx), key, dat[idx, lg]))
}




#' i18nText
#' Shiny wrapper to get internationalized text based on given key
#' Text is wrapped in HTML to make sure special characters are correctly displayed.
#' Only the text is returned
#'
#' @param key
#' @param lang the desired language
#' @param data a data.frame with keys as the first column and a column for each
#' language
#'
#' @return
#' @export
#'
#' @examples
i18nText <- function(key, lang = NULL , data = NULL) {
  shiny::HTML(geti18nValue(key, lang, data))
}




#' i18nTextOutput
#'
#' Internationalized wrapper to shiny textOutput. Based on textOutput2
#' Use only to display text
#'
#' @param id
#' @param lang
#' @param data
#' @param inline
#' @param class
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
i18nTextOutput <-
  function(id,
           lang = NULL,
           data = NULL,
           inline = FALSE,
           class = "",
           ...) {
    text <- i18nText(id, lang, data)
    textOutput2(content = text, class = class, inline = inline, ...)
  }




#' CheckQueryLanguage
#'
#'
#' @param session the user shiny session
#' @param userInfo a reactive value object where the language will
#' be stored. The language will be stored in a "lang" attribute
#'
#' @return creates an observer that checks if the language is provided in
#' the query string and if so changes the current language
#' @export
#'
#' @examples
checkQueryLanguage <- function(session, userInfo) {
  query <- shiny::isolate(shiny::parseQueryString(session$clientData$url_search))
  lang <- shiny::isolate(userInfo$lang)

  # Check if a lang parameter is provided in the query string
  # if so, change language
  if (is.null(lang) ||
      (!is.null(query$lang) && lang != query$lang)) {
    userInfo$lang <- query$lang
  }
}




#' Title
#'
#' @param input
#' @param userInfo
#' @param event
#'
#' @return
#' @export
#'
#' @examples
changeLanguageHandler <-
  function(input, userInfo, event = NULL) {
    langs <- getAllLanguages()
    lapply(langs, function(x, input, userInfo) {
      shiny::observeEvent(input[[x]], {
        userInfo$lang <- x
        if (!is.null(event)) {
          launchEvent(event, userInfo)
        }
      })
    }, input, userInfo)
  }




#' Title
#'
#' @param lang
#' @param button
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
changeLanguageOutput <- function(lang = NULL, button = TRUE, ...) {
  if (is.null(lang)) {
    langs <- getAllLanguages()
  } else {
    langs <- getAvailableLanguages(lang)
  }
  container <- if (button)
    shiny::actionButton
  else
    shiny::actionLink
  lapply(langs, function(x, lang) {
    if (is.null(lang))
      lang <- x
    container(x, i18nText(paste0("lang.", x), lang = lang), ...)
  }, lang)
}


#' Title
#'
#' @param key
#' @param lang
#' @param replace
#'
#' @return
#' @export
#'
#' @examples
i18nInsert <- function(key, lang = NULL, replace){
  str <- geti18nValue(key, lang)
  patterns <- names(replace)
  patterns <- tolower(stringr::str_replace(patterns, "(.+)$", "%\\1%"))
  names(replace) <- patterns

  stringr::str_replace_all(str, replace)
}
