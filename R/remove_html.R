#' Strip HTML from text
#'
#' Remove HTML Tags from a string
#'
#' Remove all the HTML tags from a string input
#'
#' @param string A string to clean
#'
#' @return A cleaned \code{string}
#'
#' @import checkmate
#' @export
#'
#'
#' @examples
#' \dontrun{
#' Add examples
#' }

remove_html <- function(string) {

  checkmate::assert_character(string)
  

  return(gsub("<.*?>", "", string))

}
