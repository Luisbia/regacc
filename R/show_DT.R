#' Create an HTML interactive table with DT
#'
#' @description
#'
#' An interactive HTML table with some fancy options (filter,sort,export to excel) is generated from a data frame.
#'
#' @param x a data frame
#'
#' @return DT table
#' @export
#'
#' @examples
#'
#' show_DT(dataregacc::NUTS_2021)
#'
show_DT <- function(x) {
  
  
  DT::datatable(
    x,
    caption = paste0(caption),
    filter = "top",
    class = "stripe hover",
    extensions = "Buttons",
    options = list(
      lengthMenu = list(c(20, -1), c("20", "All")),
      pageLength = 20,
      dom = "Blfrtip",
      buttons = c("excel")
    )
  )
   }
