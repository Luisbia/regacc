#' Create an HTML interactive table with DT
#'
#' @description
#'
#' An interactive HTML table with some fancy options (filter,sort,export to excel) is generated from a data frame.
#'
#' @param x a data frame
#' @param caption  a caption text for the table
#'
#' @return DT table
#' @export show_DT
#'
#' @examples
#'
#' show_DT(dataregacc::NUTS_2021)
#'
show_DT <- function(x,caption = "") {
  
  
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
