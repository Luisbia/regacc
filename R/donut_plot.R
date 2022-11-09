#' Create a donut plot for NQR or other purposes
#'
#'
#' @param value the value to show inside the donut
#' @param colour the colour of the elemtent, by default EC blue
#' @param title Title of the chart
#'
#' @return a ggplot chart
#' @export donut_plot
#'
#' @examples
#' donut_plot(value = 0.65, title = "show a percentage in a donut")
#'
donut_plot <- function(value, colour="#0E47CB",title) {

regacc::check_packages()
  # code adapted from https://rfortherestofus.com/2022/09/how-to-make-a-donut-chart-in-ggplot/
  # Wrangle data to get a data frame in the format we need it in to make our donut chart
  df <- tibble(x = 1, y = value) %>%
    mutate(y_negative = 1 - y) %>%
    pivot_longer(cols = -x)


  # Create our plot
  ggplot(df,
         aes(x = x,
             y = value,
             fill = name)) +

    # Add a bar, but don't add the legend
    geom_col(show.legend = FALSE) +

    # A pie/donut chart is a bar chart with polar coordinates
    # Add polar coordinates and set the direction to -1
    # so the filled in part starts at the top and goes clockwise
    coord_polar(theta = "y",
                direction = -1) +


    # Set the limits, which is important for adding the hole
    xlim(c(-2, 2)) +

    # Set a color scale with the highlighted section in whatever color
    # is chosen with the highlight_color argument and the rest in a light gray
    scale_fill_manual(values = c(colour, "grey90")) +

    # Set theme_void() to remove grid lines and everything else from the plot
    theme_void() +

    # Add the big number in the center of the hole
    annotate("text",
             label =  paste0(round(df %>% filter(name=="y") %>% pull(value)*100,1),"%"),
             fontface = "bold",
             color = colour,
             size = 10,
             x = -2,
             y = 0)+

    # Add title
    labs(title=paste0(title))+
    theme(plot.title = element_text(face ="bold",
          color=colour,
          size=14,
          hjust = 0.5))

}





