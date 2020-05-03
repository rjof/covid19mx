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
      mutate(FECHA_DEF = ymd(FECHA_DEF)) %>%
      mutate(FECHA_INGRESO = as.Date(FECHA_INGRESO)) %>%
      mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS))

    ## Transform integers to characters with leading zeros
    d <- transform(d
               ## , FECHA_ACTUALIZACION = FECHA_ACTUALIZACION ## Already casted
               , ORIGEN = as.character(ORIGEN)
               ## , SECTOR = SECTOR ## Numeric here and in catalogs
               , ENTIDAD_UM = sprintf(ENTIDAD_UM, fmt = "%02d")
               ## , SEXO = SEXO ## Numeric here and in catalogs
               , ENTIDAD_NAC = sprintf(ENTIDAD_NAC, fmt = "%02d")
               , ENTIDAD_RES = sprintf(ENTIDAD_RES, fmt = "%02d")
               , MUNICIPIO_RES  = sprintf(ENTIDAD_RES, fmt = "%03d")
               ## , TIPO_PACIENTE = TIPO_PACIENTE ## Numeric here and in catalogs
               ## , FECHA_INGRESO = FECHA_INGRESO ## Already casted
               ## , FECHA_SINTOMAS = FECHA_SINTOMAS ## Already casted
               ## , FECHA_DEF = FECHA_DEF ## Already casted
               ## , INTUBADO = INTUBADO ## Numeric here and in catalogs
               ## , NEUMONIA =  NEUMONIA ## Numeric here and in catalogs
               ## , EDAD = EDAD  ## Numeric here and in catalogs
               ## , NACIONALIDAD =  NACIONALIDAD ## Numeric here and in catalogs
               ## , EMBARAZO =  EMBARAZO ## Numeric here and in catalogs
               ## , HABLA_LENGUA_INDIG = HABLA_LENGUA_INDIG  ## NUMERIC HERE AND IN CATALOGS
               ## , DIABETES = DIABETES  ## NUMERIC HERE AND IN CATALOGS
               ## , EPOC = EPOC  ## NUMERIC HERE AND IN CATALOGS
               ## , ASMA = ASMA  ## NUMERIC HERE AND IN CATALOGS
               ## , INMUSUPR = INMUSUPR  ## NUMERIC HERE AND IN CATALOGS
               ## , HIPERTENSION = HIPERTENSION  ## NUMERIC HERE AND IN CATALOGS
               ## , OTRA_COM = OTRA_COM  ## NUMERIC HERE AND IN CATALOGS
               ## , CARDIOVASCULAR = CARDIOVASCULAR  ## NUMERIC HERE AND IN CATALOGS
               ## , OBESIDAD = OBESIDAD  ## NUMERIC HERE AND IN CATALOGS
               ## , RENAL_CRONICA = RENAL_CRONICA  ## NUMERIC HERE AND IN CATALOGS
               ## , TABAQUISMO = TABAQUISMO  ## NUMERIC HERE AND IN CATALOGS
               ## , OTRO_CASO = OTRO_CASO  ## NUMERIC HERE AND IN CATALOGS
               ## , RESULTADO = RESULTADO  ## NUMERIC HERE AND IN CATALOGS
               ## , MIGRANTE = MIGRANTE  ## NUMERIC HERE AND IN CATALOGS
               ## , PAIS_NACIONALIDAD = PAIS_NACIONALIDAD  ## Factor
               ## , PAIS_ORIGEN = PAIS_ORIGEN  ## Factor
               ## , UCI = UCI  ## NUMERIC HERE AND IN CATALOGS
                 )

    d <<- d
  }

#' Loads geography of municipios
#'
#' @export
#' @importFrom rgdal readOGR
#' @examples
#' load.municipios()
load.municipios <- 
  function() {
    data("municipiosmx.geojson")
    municipiosmx <<- readOGR(dsn = municipiosmx)
  }

#' Creates data frames for a state. 
#' 
#' The data frame contains FECHA_INGRESO, n = number of cases, cumsum = cumulative sum
#' 
#' @export
#' @importFrom dplyr filter group_by tally mutate
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @examples
#' ## Print plots for all states
#' data('EXAMPLECOVID19MEXICOs')
#' d = EXAMPLECOVID19MEXICO
#' timelines = grep(pattern = "timeline_.*", ls(), value = TRUE)
#' sapply(timelines, function(tl){
#'   main = gsub(pattern = "timeline_", x = tl, replacement = "")
#'   main = gsub(pattern = "_fecha_ingreso", x = main, replacement = ""  )
#'   main = gsub(pattern = "_", x = main, replacement = " "  )
#'   main = paste0(main,' - Positivo SARS-CoV-2')
#'   print(
#'   ggplot(data = get(tl), aes(x = FECHA_INGRESO,y = cumsum)) + 
#'    geom_line() +
#'    ggtitle(main) +
#'    ylab('Casos acumulados') +
#'    xlab('Fecha de ingreso a la unidad de atención')
#'    )
#'    })
timelineByState <- 
  function(stateId) {
    edo = gsub(pattern = " ", replacement = '_'
               , x = as.character(Catálogo_de_ENTIDADES [ Catálogo_de_ENTIDADES$CLAVE_ENTIDAD == stateId, 'ENTIDAD_FEDERATIVA']))
    name = paste0('timeline_'
                  , edo
                  , "_fecha_ingreso")
    d2 = d %>%
      filter(ENTIDAD_UM==stateId) %>%
      filter(RESULTADO==1) %>% # Positivo SARS-CoV-2
      group_by(FECHA_INGRESO) %>%
      tally() %>%
      mutate(cumsum = order_by(FECHA_INGRESO, cumsum(n)))
    assign(name, d2, envir = globalenv())
    }