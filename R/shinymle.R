#' SHINY app demonstrates MLE
#'
#' @returns app in a browser
#' @export
#'
#' @examples
#' \dontrun{shinymle()}
shinymle <- function(){
  shiny::runApp(system.file("SHINY/MLE", package="MATH4753F25wayne"),launch.browser = TRUE)
}
