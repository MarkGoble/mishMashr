#' safely loads source file r scripts
#'
#' A wrapper around source
#'
#' A wrapper around source. Recommended to use with the \link[mishMashr]{sibling_folder} function.
#'
#' @param source_file the R script to source
#' @param path the path to the file
#'
#' @return none
#'
#' @import checkmate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' To add!
#' }


source_loader <- function(source_file, path){
  # quick and dirty function to source scripts held in the path folder

  checkmate::assert_string(source_file)
  checkmate::assert_path_for_output(path, overwrite = TRUE)
  x <- paste0(path, "/" ,source_file)
  checkmate::assert_file_exists(x)

  source(x)

}
