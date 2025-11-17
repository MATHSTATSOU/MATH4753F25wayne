#' Strong Vs Weak LLN
#'
#' @returns Shiny app plot
#' @export
#'
#' @examples
#' \dontrun{shinyLLN()}
shinyLLN <- function(){
  shiny::runApp(system.file("LLN", package = "MATH4753F25wayne"))
}
