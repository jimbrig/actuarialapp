#' upload_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column checkboxGroupInput actionButton tags verbatimTextOutput
mod_upload_modal_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
      width = 4,
      shiny::checkboxGroupInput(
        inputId = ns("from"),
        label = "From",
        choices = c("env", "file", "copypaste", "googlesheets", "url"),
        selected = c("file", "copypaste")
      ),
      shiny::actionButton(ns("launch_modal"), "Launch modal window")
    ),
    shiny::column(
      width = 8,
      shiny::tags$b("Imported data:"),
      shiny::verbatimTextOutput(outputId = ns("name")),
      shiny::verbatimTextOutput(outputId = ns("data"))
    )
  )
  )
}

#' upload_modal Server Functions
#'
#' @noRd
#' @importFrom validate validator
#' @importFrom datamods import_modal import_server
#' @importFrom shiny moduleServer observeEvent req renderPrint
mod_upload_modal_server <- function(id, rules_config) {

  pkg_rules <- fs::path_ext_remove(basename(fs::dir_ls(fs::path_package("actuarialapp", "validation"))))

  if (rules_config %in% pkg_rules) {
    rules_config_file <- system.file(paste0("validation/", rules_config, ".yml"), package = "actuarialapp")
    stopifnot(file.exists(rules_config_file))
    rules <- validate::validator(.file = rules_config_file)
  } else {
    rules <- validate::validator(rules_config)
  }

  shiny::moduleServer(id, function(input, output, session, ruleset = rules){
    ns <- session$ns

    shiny::observeEvent(input$launch_modal, {
      shiny::req(input$from)
      datamods::import_modal(
        id = "myid",
        from = input$from,
        title = "Import data to be used in application"
      )
    })

    imported <- datamods::import_server(
      id = "myid",
      return_class = "tbl_df",
      validation_opts = list(
        rules =  ruleset
      )
    )

    output$name <- shiny::renderPrint({
      shiny::req(imported$name())
      imported$name()
    })

    output$data <- renderPrint({
      req(imported$data())
      imported$data()
    })
  })
}

## To be copied in the UI
# mod_upload_modal_ui("upload_modal_1")

## To be copied in the server
# mod_upload_modal_server("upload_modal_1")
