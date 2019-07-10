#' Efficiently find rows where a the value appears only once in the column
#'
#' Returns a data frame of values which only appear once in a df column.
#'
#' Returns only the rows from a data frame if there is only one instance of the
#' value in the specified column. This method is more efficient than grouping by
#' the column and then filtering for rows with a count == 1.
#'
#' Setting the \code{return_column_only = TRUE} will return the data frame with
#' all of the instances where the value of column appears more than once removed.
#'
#'
#' @param data_frame a data frame to read in
#' @param column the column in the data frame to check
#' @param return_column_only should the function return the whole data frame or just the column?
#'
#' @return A \code{dataframe} of values from the column which aren't duplicated.
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom purrr as_vector
#' @import checkmate
#'
#' @export
#'
#' @examples
#' mtcars$model <- rownames(mtcars)
#' # just the values from the column which appear only once
#' once_only(mtcars,"mpg")
#' # the rows for the values of the column appears once only
#' y <- once_only(mtcars,"mpg", return_column_only = FALSE )

# 3/Jul/19
# - changed sym import from rlang to dplyr

once_only <- function(data_frame, column, return_column_only = TRUE){

    checkmate::assert_data_frame(data_frame)
    checkmate::assert_string(column)
    checkmate::assert_logical(return_column_only)

    if(!column %in% colnames(data_frame)){
        stop(paste(column, "is not not a column in the data frame"))
    }

    # sym hack to allow passing in of column
    column_sym <- dplyr::sym(column)

    # determine which rows are duplicates base on column
    duplicates_logical <- duplicated(data_frame[column])

    # get the unique column values for the duplicates
    # updated to use purrr:as_vector to deal with issues when using on data_frames generated with mixed_group_x methods
    duplicates <- purrr::as_vector(unique(data_frame[duplicates_logical,column]))


    # filter out all of the rows if the value in column is appears more than once
    output <- data_frame %>%
        dplyr::filter(!(!!column_sym) %in% duplicates)

    if(return_column_only == TRUE){
        output <- output %>%
            dplyr::select(!!column_sym)
    }

    return(output)
}
