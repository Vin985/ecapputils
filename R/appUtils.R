library(shinyjs)

RANDOM_POOL <- c(1:9, letters, toupper(letters))

REQUIRED_FIELDS <- c("id", "url", "private")

readAppConf <- function(dir, file = "appconf.csv") {
  conf <-
    read.csv2(file.path(dir, file),
               stringsAsFactors = FALSE)
  missing <- REQUIRED_FIELDS[!REQUIRED_FIELDS %in% names(conf)]
  if (length(missing) > 0) {
    stop(sprintf("Error! The following columns are missing from the configuration file: %s", paste(missing, collapse = "; ")))
  }
  conf$private <- as.logical(conf$private)
  res <- setNames(split(conf[, -1], seq(nrow(conf))), conf$id)
  assign("EC_APP_CONF", res, .GlobalEnv)
  res
}

applicationLink <- function(appId, lang, button = TRUE, ...) {
  container <- if (button)
    shiny::actionButton
  else
    shiny::actionLink
  container(paste0(appId, "App"), i18nText(paste0("app.", appId), lang = lang), ...)
}


parseLanguage <- function(url, lang) {
  gsub("%LANG%", lang, url)
}

generateApplicationURL <- function(app, lang, user) {
  langArg <- ifelse(is.null(lang), "", paste0("?lang=", lang))
  nameArg <- ifelse(is.null(lang), "", paste0("?name=", user$name))
  userArg <- ifelse(is.null(user), "", paste0("&user=", generateUserToken(user)))
  url <- parseLanguage(EC_APP_CONF[[app]]$url, lang)
  paste0("window.location = '",
         url,
         langArg,
         userArg,
         "';")
}

applicationObserver <- function(appId, input, lang, user) {
    shiny::observeEvent(input[[paste0(appId, "App")]], {
      # create the javascript to redirect to the app with the selected language and user info
      js <- generateApplicationURL(appId, lang, user)
      shinyjs::runjs(js)
    }, ignoreInit = TRUE)
  }

generateUserToken <- function(user) {
  time <- format(as.numeric(user$time) * 42)
  name <- paste(strtoi(charToRaw(user$name),16L), collapse = "$")
  res <- paste(time, user$status, name, sep = "_")
}

extractUserStatus <- function(str) {
  user <- unlist(strsplit(str, "_"))
  time <- as.numeric(user[1]) / 42
  status <- as.numeric(user[2])
  name <- rawToChar(as.raw(as.numeric(unlist(strsplit(user[3], "$", fixed = TRUE)))))
  list(status = status, time = time, name = name)
}

isAdmin <- function(user, sessionTimeout = 3600) {
  return(isLogged(user, sessionTimeout) && user$status == EC_STATUS_ADMIN)
}

isLogged <- function(user, sessionTimeout = 3600) {
  if (is.null(user) || is.na(user$status) || is.na(user$time)) {
    return(FALSE)
  }
  expired <- (as.numeric(Sys.time()) - as.numeric(user$time) > sessionTimeout)
  return(!expired && user$status > EC_STATUS_GUEST)
}

#' Title
#'
#' @param query
#' @param defaultLang
#' @param availableLanguages
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
getLanguageFromQueryString <- function(query, defaultLang = NULL, availableLanguages = c("fr", "en"), ...) {
  lang <- NULL
  lg <- query[["lang"]]
  if (!is.null(lg) && lg %in% availableLanguages) {
    lang <- lg
  } else {
    if (!is.null(defaultLang))
      lang <- defaultLang
  }
  lang
}

#' Title
#'
#' @param query
#' @param sessionTimeout
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
getUserStatusFromQueryString <- function(query, ...) {
  user <- query[["user"]]
  if (!is.null(user)) {
    status <- extractUserStatus(user)
  } else {
    status <- list(name = "", status = EC_STATUS_GUEST, time = NA)
  }
  c(list(token = user), status)
}


#' getInfoFromQueryString
#'
#' Extract information from the query string. See also getLanguageFromQueryString and
#' getUserStatusFromQueryString
#'
#' @param query
#' @param args
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
getInfoFromQueryString <- function(query, args = c("lang" = getLanguageFromQueryString, "user" = getUserStatusFromQueryString), ...) {
  n <- length(args)
  res <- vector(mode = "list", length = n)
  for (i in 1:n) {
    res[[i]] <- args[[i]](query, ...)
  }
  names(res) <- names(args)
  res
}

