#' simulate_lossrun UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simulate_lossrun_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' simulate_lossrun Server Functions
#'
#' @noRd 
mod_simulate_lossrun_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_simulate_lossrun_ui("simulate_lossrun_1")
    
## To be copied in the server
# mod_simulate_lossrun_server("simulate_lossrun_1")
