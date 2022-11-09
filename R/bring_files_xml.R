
#' bring files from the server to another folder
#'
#' @param file_sel type of files ("xml", "csv",...). "xml" is the default option.
#' @param folder_sel  folder to look at (not recursively)
#' @param country_sel country to look for
#' @param folder_out folder where to store files. Default is  "data/xml"
#' @param time_min date to the file from where to start
#'
#' @return files are copied, if they exist they are not overwritten
#' @export bring_files
#'
#' @examples
#' bring_files(country_sel="PL")
bring_files<- function (file_sel = "xml",
                        folder_sel="E:/test",
                        country_sel,
                        folder_out="data/xml",
                        time_min="2022-01-01"){

  files<- list.files(path=folder_sel,
                     pattern = glob2rx(paste0("*", country_sel,"*.",file_sel,"$")),
                     recursive = FALSE,
                     full.names=TRUE) %>%
    as_tibble() %>%
    mutate(date=map(value,file.mtime)) %>%
    unnest(cols=c(date)) %>%
    filter(date> time_min) %>%
    pull(value)

  file.copy(files,folder_out,
            overwrite = FALSE,
            copy.date = TRUE)
  }

