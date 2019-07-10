#' convenience wrapper to save off object as cvs and RDS
#'
#' A convenience closure to wrap around write_out
#'
#' This function creates a function which will save off RDS and csv files to
#' named directories
#'
#' @param rds_output_directory the directory to save the RDS file to
#' @param csv_output_directory the directory to save the csv file to
#'
#' @return a \code{function} which in turn writes out an object to a csv and RDS file
#'
#' @import checkmate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # creates a function which will save RDS and csv files to the
#' # "rds_files" and  "csv_files" directories
#' x <- lazy_writer("rds_files","csv_files")
#'
#' # write out the files
#' x(mtcars)
#' }

lazy_writer <- function(rds_output_directory = NULL,
                                csv_output_directory = NULL){

  checkmate::assert_string(rds_output_directory,
                              null.ok = TRUE)

  checkmate::assert_string(csv_output_directory,
                              null.ok = TRUE)

  function(object){
    nm <-deparse(substitute(object))

    write_out(object = object,
              output_directory = rds_output_directory,
              add_date = TRUE,
              custom_name = nm)

    write_out(object = object,
              output_directory = csv_output_directory,
              add_date = TRUE,
              file_type = "csv",
              custom_name = nm)
  }
}
