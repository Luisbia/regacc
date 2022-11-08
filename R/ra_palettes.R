#' Function to create palettes from colours
#'
#' @param name name of the palette ("light","dark","primary","monoblue"...)
#' @param n number of colours
#' @param all_palettes list of all palettes
#' @param type continue or discrete
#'
#' @return a ready to use palette
#' @export ra_palettes
#'
#' @examples
#' scales::show_col(ra_palettes(name="light"))
ra_palettes <- function(name, n, all_palettes = ra_colours, type = c("discrete", "continuous")) {

  ra_colours = list(
    light = c("#2644A7", "#AF155C", "#208486", "#AA5F18", "#B656BD","#388AE2","#E04040", "#33A033","#672DC4","#B39421"),
    dark = c("#588CFB", "#D3659F","#4B8C8A","#B06923", "#B656BD", "#75BDFC","#E65050","#73C472","#8E61F8","#D8AC28"),
    primary = c("#0E47CB","#FFCC00"),
    monoblue = c("#082B7A", "#0B39A2" , "#0E47CB", "#6E91E0", "#CFDAF5"),
    monofuchsia = c("#241125", "#6D3371", "#B656BD", "#D399D7", "#F0DDF1" )

  )

    palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}
