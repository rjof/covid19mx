#' Baja los datos sobre covid de la Secretaría de salud
#'
#' @export
#' @importFrom utils download.file unzip
#' @name %>%
#' @rdname pipe
#' @examples
#' downloadCovidmx()
downloadCovidmx <- function() {
  dir.create(file.path("./", "data"), showWarnings = FALSE)
  download.file(
    url = "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
    , destfile = "./data/datos_abiertos_covid19.zip")
  unzip(zipfile = "./data/datos_abiertos_covid19.zip", exdir = "./data/")
}

#' Baja el diccionario de datos de la Secretaría de salud
#'
#' @export
#' @importFrom utils download.file unzip
#' @examples
#' downloadDicHealth()
downloadDicHealth <- function() {
  dir.create(file.path("./", "data"), showWarnings = FALSE)
  download.file(url = "http://187.191.75.115/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip"
                , destfile = "./data/diccionario_datos_covid19.zip")
  unzip(zipfile = "./data/diccionario_datos_covid19.zip", exdir = "./data/")
}

#' Prepares catalogs in local variables
#'
#' @export
#' @importFrom xlsx getSheets loadWorkbook
#' @importFrom readxl read_excel
#' @examples
#' varDicHealth()
varDicHealth <- function() {
  catalogosFilename = list.files("./data", recursive = TRUE, pattern = "Catalogos.*xlsx", full.names = TRUE)
  descriptoresFilename = list.files("./data", recursive = TRUE, pattern = "Descriptores.*xlsx", full.names = TRUE)
  
  catalogosSheets <- xlsx::getSheets(xlsx::loadWorkbook(catalogosFilename))
  catalogosSheetsNames = gsub(pattern = " "
                              , replacement = "_"
                              , x = names(catalogosSheets))
  
  for ( i in 1:length( catalogosSheets )) {
    assign(catalogosSheetsNames[i]
           , read_excel( catalogosFilename
                         , sheet = names(catalogosSheets[i]))
           , envir = globalenv() )
  }
  
  descriptoresSheets <- getSheets(loadWorkbook(descriptoresFilename))
  descriptoresSheetsNames = gsub(pattern = " "
                                 , replacement = "_"
                                 , x = names(descriptoresSheets))
  for ( i in 1:length(descriptoresSheets)) {
    descriptoresName = paste0("descriptoresSheet",descriptoresSheetsNames[i])
    assign(descriptoresName, read_excel(descriptoresFilename
                                        , sheet = names(descriptoresSheets[i]))
           , envir = globalenv() )
  }
  
}

