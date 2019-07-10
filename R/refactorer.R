#' easier way to refactor using lists
#'
#' Refactor a factor using input lists
#'
#' This function allows for quick bulk replacement of the labels of a factor
#' rather than the more complex forcats way...
#'
#' @param data_frame The data frame containing the column
#' @param column The column to refactor
#' @param level_vector The levels in the orignal column
#' @param label_vector The labels to apply
#' @param other_label The label to use for 'other' values (e.g. unknowns)
#'
#' @return an updated column
#'
#' @importFrom forcats fct_explicit_na
#' @importFrom magrittr %>%
#' @import dplyr
#' @import checkmate
#' @importFrom rlang :=
#'
#' @export
#'
#' @examples
#' \dontrun{
#' To add!
#' }

# 3/Jul/19
# - changed sym import from rlang to dplyr

refactorer <- function(data_frame,
                       column,
                       level_vector,
                       label_vector,
                       other_label = "Unknown"){

  checkmate::assert_data_frame(data_frame)
  checkmate::assert_string(column)
  checkmate::assert_vector(level_vector)
  # check the two vectors are of the same length
  level_vector_len <- length(level_vector)
  checkmate::assert_vector(label_vector, len = level_vector_len)
  checkmate::assert_string(other_label)

  if(!column %in% colnames(data_frame)){
    stop(paste(column, "is not not a column in the data frame"))
  }


  level_vector <- tolower(level_vector)
  column_sym <- dplyr::sym(column)
  # note the special syntax i.e. !!column_sum := ...
  # https://www.tidyverse.org/articles/2018/03/dplyr-0.2.0/
  data_frame %>%
    dplyr::mutate(!!column_sym := trimws(!!column_sym)) %>% #get rid of whitespace...
    dplyr::mutate(!!column_sym := tolower(!!column_sym)) %>% # change to lower case for better comparisons..
    dplyr::mutate(!!column_sym := factor(!!column_sym,
                                  levels = level_vector,
                                  labels = label_vector)) %>%
    dplyr::mutate(!!column_sym := forcats::fct_explicit_na(!!column_sym,
                                           na_level =  other_label)) # use forcats to deal with unset levels...

}
