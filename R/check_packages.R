#' Check packages
#'
#' The function checks that the packages needed are installed and loads them
#'
#' @export check_packages
#'
#' @examples
#' check_packages()
check_packages<- function(){

  # Specify the list of required packages to be installed and load
  Required_Packages=c("rio",
                      "magrittr",
                      "ggplot2",
                      "dplyr",
                      "tidyr",
                      "tibble",
                      "purrr",
                      "stringr",
                      "data.table",
                      "lubridate",
                      "janitor",
                      "eurostat",
                      "DT",
                      "openxlsx",
                      "readsdmx",
                      "vroom",
                      "scales",
                      "splitstackshape",
                      "ragg",
                      "patchwork",
                      "png",
                      "rlang",
                      "ggtext")
  # Function to Install and Load R Packages
  Install_And_Load <- function(Required_Packages)
  {
    Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];

    if(length(Remaining_Packages))
    {
      install.packages(Remaining_Packages);
    }
    for(package_name in Required_Packages)
    {
      library(package_name,character.only=TRUE,quietly=TRUE);
    }
  }

  Install_And_Load(Required_Packages)

}
