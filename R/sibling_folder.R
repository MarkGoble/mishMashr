#' returns the full path for a sibling folder
#'
#' A quick and easy way to get a sibling folder
#'
#' A quick function to access a sibling folder (i.e. a folder with the same parent folder).
#' If parent_level is set as FALSE (defaults to false) it will look on the same level in
#' the directory structure; however if parent_level is set to TRUE then it will look for the
#' sibling folder at the parent level (I hope that makes sense!)
#'
#' @param destination_folder the destination folder you want to reach (in quotes)
#' @param parent_level if true then return the sibling of the parent folder (default = false)
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

sibling_folder <- function(destination_folder, parent_level = FALSE){

  checkmate::assert_string(destination_folder)

  if(parent_level == FALSE){
    x <- paste0(dirname(getwd()),"/", destination_folder)

  }

  if(parent_level == TRUE){
    x <- paste0(dirname(dirname(getwd())),"/", destination_folder)
  }

  checkmate::assert_directory_exists(x)
  return(x)
}



