#' consistently write out data files
#'
#' A wrapper around write.csv and saveRDS
#'
#' This function provides consistent saving. By default the file will be saved
#' with the name of the name of the object.
#'
#' The format of the date is _(day)_(month)_(year)
#'
#' @param object the object to be saved
#' @param add_date should the date be added to the end of the file name?
#' @param output_directory should the output be saved to a particular directory (default to working directory)
#' @param file_type should the file be saved as RDS or csv (default RDS)
#' @param custom_name should a custom name be used instead of the objects' name
#'
#' @return none
#'
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate year
#' @import checkmate
#' @importFrom stringr str_sub
#' @importFrom utils write.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  write_out(mtcars)
#'  # this will save the mtcars data to an RDS file.
#'
#'  write_out(mtcars, add_date = TRUE, file_type = "csv", custom_name = "cars")
#'  # writes the mtcars data to a csv file "cars_(day)_(month)_(year).csv"
#'
#' }

write_out <- function(object,
                      add_date = FALSE,
                      output_directory = NULL,
                      file_type = "RDS",
                      custom_name = NULL){

  checkmate::assert_string(output_directory, null.ok = TRUE)

  if(!is.null(output_directory)){
  } else{
    output_directory <- getwd()
  }

  checkmate::assert_logical(add_date)
  checkmate::assert_path_for_output(output_directory,
                                    overwrite = TRUE)
  checkmate::assert_choice(tolower(file_type), c("rds", "csv"))
  # TODO - tighten to pattern match valid filenames
  checkmate::assert_string(custom_name,
                              null.ok = TRUE)


  if(!is.null(custom_name)) {
    object_name <- custom_name
  } else {
    object_name <- deparse(substitute(object))
  }

  if(add_date == TRUE){
    x <- Sys.Date()
    date_string = paste0("_",
                         stringr::str_sub(paste0("00",lubridate::day(x)), -2,-1),
                         "_",
                         stringr::str_sub(paste0("00",lubridate::month(x)), -2,-1),
                         "_",
                         lubridate::year(x))
  } else {
    date_string = ""
  }

  if(!is.null(output_directory)){
    output_directory <- paste0(output_directory,"/")
  }

  if(tolower(file_type) == "rds"){
    filename <- paste0(output_directory,object_name,date_string,".RDS")

    saveRDS(object = object,
            file = filename)
  } else if (tolower(file_type) == "csv") {
    filename <- paste0(output_directory,object_name,date_string,".csv")
    utils::write.csv(x = object,
              file = filename,
              na="",
              row.names = FALSE)
  }

}
