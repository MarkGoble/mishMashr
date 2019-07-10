#' returns the full path for a sibling folder
#'
#' A quick and easy way to get a sibling folder
#'
#' A quick function to access a sibling folder (i.e. a folder with the same parent folder).
#'
#' @param destination_folder the desination folder you want to reach (in quotes)
#'
#' @return the full path to the destination folder
#'
#' @import checkmate
#' @export
#'
#' @examples
#' \dontrun{
#' To add!
#' }

sibling_folder <- function(destination_folder){

  checkmate::assert_string(destination_folder)

  x <- paste0(dirname(getwd()),"/", destination_folder)
  checkmate::assert_directory_exists(x)

  return(x)
}
