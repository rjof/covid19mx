#' Baja el diccionario de datos de la Secretaría de salud
#'
#' @export
#' @importFrom utils download.file unzip
#' @examples
#' download.diccionario()
download.diccionario <- 
  function() {
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
#' variables.diccionario()
variables.diccionario <- 
  function() {
    catalogosFilename = list.files("./data", recursive = TRUE, pattern = "Catalogos.*xlsx", full.names = TRUE)
    descriptoresFilename = list.files("./data", recursive = TRUE, pattern = "Descriptores.*xlsx", full.names = TRUE)
    
    ## library(xlsx)
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

#' Baja los datos sobre covid de la Secretaría de salud
#'
#' @export
#' @importFrom utils download.file unzip
#' @name %>%
#' @rdname pipe
#' @examples
#' download.covidmx()
download.covidmx <- 
  function() {
    dir.create(file.path("./", "data"), showWarnings = FALSE)
    download.file(
      url = "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
      , destfile = "./data/datos_abiertos_covid19.zip")
    unzip(zipfile = "./data/datos_abiertos_covid19.zip", exdir = "./data/")
  }

#' Prepares data frame of covidmx data
#'
#' @export
#' @importFrom utils read.csv2
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @examples
#' data.frame.covidmx()
data.frame.covidmx <- 
  function() {
    datosFilename = list.files("./data", recursive = TRUE, pattern = ".*COVID19MEXICO.csv", full.names = TRUE)
    d = read.csv2(file = datosFilename, sep=',',stringsAsFactors = TRUE)
    d <- d %>%
      mutate(FECHA_ACTUALIZACION = as.Date(FECHA_ACTUALIZACION)) %>%
      mutate(FECHA_DEF = lubridate::ymd(FECHA_DEF)) %>%
      mutate(FECHA_INGRESO = as.Date(FECHA_INGRESO)) %>%
      mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS))
    d <<- d
  }

#' Carga los datos geográficos de los municipios de México
#'
#' @export
#' @examples
#' load.municipios()
load.municipios <- 
  function() {
    data("municipiosmx.geojson")
    municipiosmx <<- rgdal::readOGR(dsn = municipiosmx)
  }
