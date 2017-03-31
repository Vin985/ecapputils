

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rhandsontable)

BACKUP_DIR <- "backupDir"
BACKUP_NB <- 5
TMP_DIR <- "tmp"
AUTOSAVE_INTERVAL <- 60000 # Autosave interval in milliseconds

fileLinksOutput <- function(apps) {
  shiny::renderUI({
    shiny::fluidRow(
    lapply(apps, function(x){
      shiny::actionButton(x, x)
    }))
  })
}

fileLinksHandler <- function(app, path, input, info) {
  shiny::observeEvent(input[[app]] ,{
    info$app <- app
    info$path <- path
    if (file.exists(file.path(path, TMP_DIR, paste0(app, ".tmp")))) {
      info$loadTmp <- TRUE
    } else {
      loadData(file.path(path, paste0(app, ".csv")), info)
    }
  })
}

## get application based on file path
getApp <- function(path){
  file <- basename(path)
  return(sub("\\..+", "", file))
}

## Load data and fill
loadData <- function(path, info) {
    print(paste("loading file: ", path))
    info$data <- read.csv2(
      path,
      header = TRUE,
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    )
    info$previous <- shiny::isolate(info$data)
    info$previoustime <- format(Sys.time(), format = "%s")
}

writeData <- function(data, path){
  write.csv2(data,
             path,
             row.names = FALSE,
             fileEncoding = "UTF-8")
}


loadTmpModal <- function() {
  modalDialog(tagList(
    actionButton("loadTmp", "Yes, load the working file"),
    actionButton("loadFile", "No, load the original file instead")),
    title = "A more recent working file has been found. Load it instead?",
    footer = NULL
  )
}

removeTemp <- function(path, app) {
  destFile <- file.path(path, TMP_DIR, paste0(app, ".tmp"))
  if (file.exists(destFile)){
    file.remove(destFile)
  }
}

editCsvServer <- function(path) {

  shiny::shinyServer(function(input, output, session) {
    info <- shiny::reactiveValues()

    if (!is.null(path)) {
      # Check if path exists and is a dir
      path.info <- file.info(path)

      if (is.na(path.info$size)) {
        # file doesn't exist, select file instead
      } else {
        # Check if path is a directory
        if (path.info$isdir) {
          # List all csv files
          files <- list.files(path, pattern = "*.csv")
          apps <- sub("\\..+", "", files)
          lapply(apps, fileLinksHandler, path, input, info)
          output$selectFile <- fileLinksOutput(apps)
        } else {
          files <- path
          loadData(path, info)
        }
      }
    } else {
      # allow user to load his file
      info$path <- getwd()
      info$app <- "lang"
      info$selectFile <- TRUE
      output$selectFile <- shiny::renderUI({
        shiny::fileInput("file", "Import file")
      })

    }

    ## Load data
    shiny::observeEvent(input$file, {
      path <- input$file
      if (is.null(path))
        return(NULL)
      loadData(path$datapath, info)
    })

    ## Handsontable
    shiny::observe({
      if (!is.null(input$data)) {
        data <- rhandsontable::hot_to_r(input$data)
        info$data <- data
        info$toSave <- TRUE
      }
    })


    ## Add column
    shiny::observeEvent(input$addCol, {
      if (is.null(input$colName) || input$colName == "") {
        output$errorCol <-
          shiny::renderText({
            "You must enter a column name"
          })
      } else {
        data <- shiny::isolate(info$data)
        if (!is.null(data)) {
          # take the first column as a template
          newCol <- data[, 1]
          # Empty it
          newCol <- ""
          # add it at the end
          data[[input$colName]] <- newCol
        } else {
          data <- data.frame("", stringsAsFactors = FALSE)
          colnames(data) <- input$colName
        }

        info$data <- data
      }
    })

    ## Add row
    shiny::observeEvent(input$addRow, {
      data <- shiny::isolate(info$data)

      if (is.null(data)) {
        output$errorRow <-
          shiny::renderText({
            "Please add a new column first"
          })
      } else {
        # take the first row as a template
        new <- rbind(data, data[1,])
        # Empty it
        new[nrow(new),] <- ""
        rownames(new) <- seq(1, nrow(new))
        info$data <- new
      }
    })

    ## Render data
    output$data <- rhandsontable::renderRHandsontable({
      if (!is.null(info$data)) {
        rhandsontable::rhandsontable(
          info$data,
          stretchH = "all",
          manualColumnResize = TRUE,
          colWidths = rep(150, ncol(info$data))
        )
      }
    })

    ## Download data
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        app <- shiny::isolate(info$app)
        paste0(app, ".csv")
      },
      content = function(file) {
        d <- shiny::isolate(info$data)
        if (!is.null(d)) {
          writeData(d, file)
          ## Remove temp file
          path <- shiny::isolate(info$path)
          app <- shiny::isolate(info$app)
          removeTemp(path, app)
        }
      }
    )

    ## Save data
    shiny::observeEvent(input$saveData, {
        destPath <- shiny::isolate(info$path)
        app <- shiny::isolate(info$app)
        d <- shiny::isolate(info$data)

        # Need to create  backup
        if (input$backupData) {
          nBackup <- 5
          destDir <- dirname(destPath)
          backupDir <- file.path(destDir, BACKUP_DIR)
          # Check if backup dir exist. Create otherwise
          if (!dir.exists(backupDir)) {
            dir.create(backupDir, mode = "0644")
          }
          # List all backup files, remove the oldest, keep only 2
          backupFiles <- sort(list.files(backupDir, paste0(app, ".bak")), decreasing = TRUE)
          bakFile <- file.path(backupDir,
                               paste0(shiny::isolate(info$previoustime),
                                      app, ".bak"))
          if (!file.exists(bakFile)) {
            if (length(backupFiles) > (BACKUP_NB - 1)) {
              toRemove <- backupFiles[BACKUP_NB:length(backupFiles)]
              print(paste0("removing: ", toRemove))
              lapply(file.path(backupDir,toRemove), file.remove)
            }
            writeData(shiny::isolate(info$previous), bakFile)

          } else {
            print("Backup file exists, do not save again")
          }
        }

        ## Remove temp file
        removeTemp(destPath, app)

        ## Save and overwrite data
        writeData(d, file.path(destPath, paste0(app, ".csv")))
        output$saved <- renderText({
            paste0("File saved on ", Sys.time())
        })
    })

    ## Autosave
    observe({
      ## Check every AUTOSAVE_INTERVAL
      shiny::invalidateLater(AUTOSAVE_INTERVAL, session)
      ## Data has changed and autosave is on
      if (shiny::isolate(req(info$toSave)) && input$autosave) {
          app <- shiny::isolate(info$app)
          path <- shiny::isolate(info$path)
          # Check if backup dir exist. Create otherwise
          tmpDir <- file.path(path, TMP_DIR)
          if (!dir.exists(tmpDir)) {
            dir.create(tmpDir, mode = "0644")
          }
          tmpFile <- file.path(tmpDir, paste0(app, ".tmp"))
          writeData(shiny::isolate(info$data), tmpFile)
          output$autosave <-
            renderText({
              paste0("Draft saved on ", Sys.time())
          })
          info$lastAuto <- Sys.time()
          info$toSave <- FALSE
      }
    })

    ## Load temp file dialog
    observe({
      if (req(info$loadTmp)) {
      showModal(loadTmpModal())
      }
    })

    ## Load temp file
    observeEvent(input$loadTmp, {
      path <- shiny::isolate(info$path)
      app <- shiny::isolate(info$app)
      loadData(file.path(path, TMP_DIR, paste0(app, ".tmp")), info)
      shiny::removeModal()
      info$loadTmp <- FALSE
    })

    observeEvent(input$loadFile, {
      path <- shiny::isolate(info$path)
      app <- shiny::isolate(info$app)
      loadData(file.path(path, paste0(app, ".csv")), info)
      shiny::removeModal()
      info$loadTmp <- FALSE
    })

  })
}

editCsvUI <- function() {
  shiny::shinyUI(shiny::fluidPage(
    shiny::titlePanel("Edit CSV file"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        style = "position:fixed; top:9%; width:24%",
        shiny::uiOutput("selectFile"),
        htmltools::br(),
        shiny::fluidRow(
          shiny::textOutput("errorRow"),
          shiny::actionButton("addRow", "Add a new row")
        ),
        htmltools::br(),
        shiny::fluidRow(
          shiny::textOutput("errorCol"),
          shiny::textInput("colName", "Enter column name"),
          shiny::actionButton("addCol", "Add a new column")
        ),
        htmltools::br(),
        shiny::fluidRow(shiny::downloadButton("downloadData", "Download data")),
        htmltools::br(),
        shiny::fluidRow(shiny::checkboxInput("autosave", "Autosave your progress", value = FALSE)),
        textOutput2("autosave", style = "font-size: 11px; color:blue;"),
        htmltools::div(class = "row", style = "font-size: 11px; color:red;",
             "Warning! the existing file will not be overwritten. Please press on the save button below to apply your changes"),
        htmltools::br(),
        shiny::fluidRow(shiny::checkboxInput("backupData", "Create backup file", value = FALSE)),
        htmltools::div(class = "row", style = "font-size: 11px; color:blue;",
          "Keep a backup of the loaded file. Up to 5 backup files per application will be kept"),
        htmltools::br(),
        shiny::fluidRow(shiny::actionButton("saveData", "Save data")),
        htmltools::div(class = "row", style = "font-size: 11px; color:red;",
             "Warning, the existing file will be overwritten. Download data or create a backup if you want to keep a previous version"),
        textOutput2("saved", style = "font-size: 11px; color:blue;")
      ),

      # Application title
      shiny::mainPanel(
        width = 9,
        htmltools::div(style = "width:100%; margin-bottom:80px", rhandsontable::rHandsontableOutput("data"))
      )
    )
  ))
}

#' Title
#'
#' @param path 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
editCsvApp <- function(path = NULL, ...) {
  shiny::runApp(list(ui = editCsvUI(), server = editCsvServer(path)), ...)
}
