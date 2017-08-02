library(shinyjs)


CONF_VAR <- "EC_APP_CONF"
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
  assign(CONF_VAR, res, .GlobalEnv)
  res
}

applicationLink <- function(appId, lang, button = TRUE, ...) {
  container <- if (button)
    shiny::actionButton
  else
    shiny::actionLink
  container(paste0(appId, "App"), i18nText(paste0("app.", appId), lang = lang), ...)
}


applicationObserver <- function(appId, input, lang) {
    conf <- get(CONF_VAR)
    shiny::observeEvent(input[[paste0(appId, "App")]], {
      # create the javascript to redirect to the app with the selected language
      js <-
        paste0("window.location = '", conf[[appId]]$url,
               "?lang=", lang , "';")
      shinyjs::runjs(js)
    })
  }
