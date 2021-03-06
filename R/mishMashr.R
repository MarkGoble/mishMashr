#'  Little helpers to make life easier...
#'
#' A collection of general helper functions to make it easier to look for data
#' quality issues and some helpers for saleforce analysis
#'
#' @docType package
#' @name mishMashr
#' @author Mark Goble \email{mark.goble@@goble.co.uk}
"_PACKAGE"

# added to resolve warning no visible binding for global variable ‘.’
# see: https://github.com/tidyverse/magrittr/issues/29
# version 0.1.1 - added bulk_in functionality
# version 0.1.2 - added the ability to look at a parent in sibling_folder
#               - tidy-up of help files pointing to old packages

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

