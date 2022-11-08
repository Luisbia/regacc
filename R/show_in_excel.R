#'Export to excel a data frame
#'
#'@description
#'
#'For closer inspection of small data frames this functions exports it to excel and opens the file.
#' @param .data
#'
#' @return an excel file in a temporary folder
#' @export
#'
#' @examples
#'
#' show_in_excel(dataregacc::NUTS_2021)
show_in_excel <- function(.data){

  tmp <- paste0(tempfile(), ".xlsx")
  openxlsx::write.xlsx(.data,tmp)
  browseURL(tmp)
}
