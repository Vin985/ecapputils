library(shinyjs)


CONF_VAR <- "EC_APP_CONF"

readAppConf <- function(dir, file = "app.conf") {
  conf <-
    read.table(file.path(dir, file),
               sep = "=",
               stringsAsFactors = FALSE)
  # create a list of key/values from file
  res <- as.list(conf$V2)
  names(res) <- conf$V1
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
        paste0("window.location = '", conf[[appId]],
               "?lang=", lang , "';")
      shinyjs::runjs(js)
    })
  }
