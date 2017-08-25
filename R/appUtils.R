library(shinyjs)

RANDOM_POOL <- c(1:9, letters, toupper(letters))

REQUIRED_FIELDS <- c("id", "url", "private")

EC_STATUS_GUEST <- 0
EC_STATUS_USER <- 1
EC_STATUS_ADMIN <- 2

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
  userArg <- ifelse(is.null(user), "", paste0("&user=", user$token))
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

generateUserStatus <- function(status) {
  random <- c(sample(RANDOM_POOL, 10, replace = TRUE), status)
  res <- paste(random, collapse = "")
}

extractUserStatus <- function(str) {
  l <- nchar(str)
  res <- substr(str, l, l)
  as.numeric(res)
}

isAdmin <- function(user) {
  return(user$status == EC_STATUS_ADMIN)
}

isLogged <- function(user) {
  return(!is.null(user) && !is.na(user$status) && (user$status == EC_STATUS_USER || user$status == EC_STATUS_ADMIN))
}

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

getUserStatusFromQueryString <- function(query, default = EC_STATUS_GUEST, ...) {
  user <- query[["user"]]
  if (!is.null(user)) {
    status <- extractUserStatus(user)
  } else {
    token <- ""
    status <- default
  }
  list(token = user, status = status)
}

getInfoFromQueryString <- function(query, args = c("lang" = getLanguageFromQueryString, "user" = getUserStatusFromQueryString), ...) {
  n <- length(args)
  res <- vector(mode = "list", length = n)
  for (i in 1:n) {
    res[[i]] <- args[[i]](query, ...)
  }
  names(res) <- names(args)
  res
}

