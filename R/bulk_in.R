#' Efficiently bulk load a directory of csv or excel files
#'
#' Bulk loads csv or excel files contained in a directory into a dataframe
#'
#' This function recursively finds all csv / excel files in a folder and loads
#' them all into a single dataframe. Note will only deal with either excel or
#' csv not a mixture.

#'
#' @param input_directory the directory to import the files from
#' @param type the type of input file ("csv" or "excel")
#' @param sheet_name (optional) name of the excel sheet to import (same in all files) default = NULL
#' @param safe_mode should safemode be used? (default = FALSE)
#'
#' @return A \code{dataframe} of containing all of the data from the files
#'
#' @import checkmate
#' @importFrom utils read.csv
#' @importFrom readxl read_excel
#' @importFrom data.table rbindlist
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # to add
#' }
#' @details Uses the read_excel function from readxl for excel import  - if no
#' sheet_name is provided (or is NULL) then will read in the first sheet by
#' default. The column types will be guessed when the data is read in.
#'
#' The package uses data.table's rbindlist function for performance reasons.
#' This is fast but binds based on column position rather than matching column
#' names. If this causes issues then use safe_mode = TRUE which uses a slower
#' match based on column names.


bulk_in <- function(input_directory, type, sheet_name = NULL, safe_mode = FALSE){

  # Inbound field validation
  checkmate::assert_string(input_directory)
  checkmate::assert_directory_exists(input_directory)
  # check that the type specified is either csv or excel (setting as lower case)
  checkmate::assert_choice(tolower(type), c("csv","excel"))
  # check the name of the sheet - note this is a lightweight check we're not
  # checking that the same name appears on all sheets
  checkmate::assert_string(sheet_name, null.ok = TRUE)
  checkmate::assert_logical(safe_mode)


if(type=="csv"){
  # set the regex pattern to find csv files (the [^~] excludes tempfiles beginning with '~')
  file_pattern <- "^[^~]*.csv$"
} else {
  # set the regex pattern to find xls or xlsx files (the [^~] excludes tempfiles beginning with '~')
  # this version won't handle xlsm files
  file_pattern <- "^[^~]*.xlsx?$"
}

all_dirs <-list.dirs(input_directory, recursive = TRUE)

all_files <- unlist(lapply(all_dirs,list.files,
                           pattern = file_pattern,
                           full.names = TRUE ))

if(type=="csv"){
  input_list <- lapply(all_files,function(i){utils::read.csv(i, header=TRUE, na.strings = "")})
} else{
  input_list <- lapply(all_files,function(i){readxl::read_excel(i, sheet=sheet_name, col_names=TRUE)})
}

if(safe_mode==TRUE){
  df <- do.call(rbind, input_list)
} else {
  df <- data.table::rbindlist(input_list)
}



return(df)

}
