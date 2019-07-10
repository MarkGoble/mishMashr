#' Find parents with two or more different child values in the child column
#'
#' Produces a list of parents which have more than one distinct child value and the number of
#' children. It will filter out any parents with only one child value.
#' This is useful in finding issues where the same parent has different values on the children :
#' for example where all products should have the same colour description
#'
#' @param data_frame a data frame to check
#' @param parent_column the parent column on the data frame
#' @param child_column the child column to check
#'
#' @return A \code{data frame} containing parents where there is more than 1 child value and the number of occurances.
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import checkmate
#' @importFrom purrr as_vector
#'
#' @export
#'
#' @examples
#' mixed_group_check(mtcars,"vs","cyl")

# 03/Jul/19
# - changed sym import from rlang to dplyr
# - added purrr::as_vector to cope with inconsistencies in type return from unique.

mixed_group_check <- function(data_frame, parent_column, child_column){

  checkmate::assert_data_frame(data_frame)
  checkmate::assert_string(parent_column)
  checkmate::assert_string(child_column)

  if(!child_column %in% colnames(data_frame)){
    stop(paste(child_column, "is not not a column in the data frame"))
  }

  if(!parent_column %in% colnames(data_frame)){
    stop(paste(parent_column, "is not not a column in the data frame"))
  }

  # sym hack to allow passing in of column
  child_column_sym <- dplyr::sym(child_column)
  parent_column_sym <- dplyr::sym(parent_column)


  x <- data_frame %>%
    dplyr::select(!!parent_column_sym, !!child_column_sym) %>%
    unique() %>%
    dplyr::select(!!parent_column_sym)

  y <- duplicated(x[parent_column])

  # 03/jul/19 - added purrr::as_vector to cope with inconsistencies in unique return
  z <- purrr::as_vector(unique(x[y,]))

  zz <- data_frame %>%
    dplyr::filter(!!parent_column_sym %in% z) %>%
    dplyr::select(!!parent_column_sym, !!child_column_sym) %>%
    dplyr::arrange(!!parent_column_sym)

  zz %>%
    dplyr::group_by(!!parent_column_sym, !!child_column_sym) %>%
    dplyr::count() %>%
    dplyr::ungroup()
}
