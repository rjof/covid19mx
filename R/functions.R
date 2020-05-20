#' Downloads data from hospitals in the metropolitan area of Mexico City
#'
#' @export
#' @examples
#' downloadHospitalsZmcm()
downloadHospitalsZmcm <- function(year = NULL) {
    dir.create(file.path("./", "data"), showWarnings = FALSE)
    url = "https://datos.cdmx.gob.mx/explore/dataset/capacidad-hospitalaria/download/?format=csv&"
    destfile = "./data/capacidadHospitalariaZmcm"
    if ( !is.null(year) ) {
      if (is.numeric(year) & nchar(year) == 4) {
        url = paste0(url, "refine.fecha=", year, "&")
        destfile = paste0(destfile, "-", year)
      } else {
        e <- simpleError("parameter year should integer of four digits.")
        stop(e)
      }
    }
    url = paste0(url, "timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C")
    destfile = paste0(destfile, ".csv")
    download.file(url = url, destfile = destfile)
  }

#' Preprocess capacidadHospitalariaZmcm
#' 
#' @importFrom dplyr mutate rowwise
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
prepCHZmcm <- function(df){
  # df = read.csv2(file = './data/capacidadHospitalariaZmcm.csv', sep=',',stringsAsFactors = FALSE)
  names = names(df)
  df$Coordenadas = as.character(df$Coordenadas)
  df = df %>% 
    mutate(
      splits = strsplit(Coordenadas, ",")
    ) %>% 
    dplyr::rowwise() %>% 
    mutate(
      lat = splits[1],
      lon = splits[2]
    )
  df = df[c(names,'lat','lon')]
  
  df <- transform(
    df
    ,Fecha = as.Date(Fecha)
    ## Nombre_hospital
    ,Institucion = factor(Institucion, ordered = TRUE, 
                          levels = c("","EDOMEX","IMSS","ISSSTE","SEDESA","SSA"))
    ,Estatus_capacidad_hospitalaria = factor(Estatus_capacidad_hospitalaria,
                                             levels = c("Buena", "Media", "Crítica" , ""))
    ,Estatus_capacidad_UCI = factor(Estatus_capacidad_UCI,
                                    levels = c("Buena", "Media", "Crítica" , ""))
    #"Coordenadas"
    ,lat = as.numeric(lat)
    ,lon = as.numeric(lon)
  )
  df
}

#' 
#' Downloads data with hospitalized persons in CDMX & EdoMex
#'
#' @export
#' @examples
#' downloadHospitalizedCdmxEdoMex(month = "enero", year = 2020)
downloadHospitalizedCdmxEdoMex <- function(year = NULL, month = NULL) {
    dir.create(file.path("./", "data"), showWarnings = FALSE)
    months = c('enero','febrero','marzo','abril','mayo','junio','julio','agosto','septiembre','octubre','noviembre','diciembre')
    url = "https://datos.cdmx.gob.mx/explore/dataset/personas-hospitalizadas-covid19/download/?format=csv&"
    destfile = "./data/personas-hospitalizadas-covid19-Zmcm"
    if ( !is.null(month) ) {
      if (month %in% months) {
        url = paste0(url, "refine.mes=", month, "&")
        destfile = paste0(destfile, "-", month)
      } else {
        e <- simpleError("Parameter month should be one of the above")
        print(months)
        stop(e)
      }
    }
    if ( !is.null(year) ) {
      if (is.numeric(year) & nchar(year) == 4) {
        url = paste0(url, "refine.fecha=", year, "&")
        destfile = paste0(destfile, "-", year)
      } else {
        e <- simpleError("parameter year should integer of four digits.")
        stop(e)
      }
    }
    url = paste0(url, "timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C")
    destfile = paste0(destfile,".csv")
    download.file(url = url, destfile = destfile)
  }

#' Downloads governmental policies related to the covid19 epidemic
#'
#' @export
#' @examples
#' downloadPolicies()
downloadPolicies <- function(year = NULL) {
    dir.create(file.path("./", "data"), showWarnings = FALSE)
    url = "https://datos.cdmx.gob.mx/explore/dataset/inventario-medidas-contingencia-covid19/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C"
    destfile = "./data/medidasContingencia.csv"
    download.file(url = url, destfile = destfile)
  }

#' Downloads SINAVE database (suspicious cases)
#'
#' @export
#' @importFrom RCurl getURL
#' @param numRows Numeric. Number of rows to download. Defaults to -1 for all
#' @examples
#' downloadSINAVE(3)
downloadSINAVE <- function(numRows = -1) {
  dir.create(file.path("./", "data"), showWarnings = FALSE)
  sinave = getURL(paste0(
    "https://datos.cdmx.gob.mx/api/v2/catalog/datasets/base-covid-sinave/exports/csv?"
    ,"rows=",numRows
    ,"&select=exclude(geo_shape)&timezone=UTC&delimiter=%3B"))
  write.table(x = sinave, file = './data/covidmx-sinave.csv', row.names = FALSE, sep = ";", col.names = FALSE, quote = FALSE)
}

#' Downloads the dictinary of the SINAVE database
#'
#' Baja el diccionario de datos de la Secretaría de salud desde el sitio de la CDMX
#' @export
downloadDictionarySINAVE <- function() {
  dir.create(file.path("./", "data"), showWarnings = FALSE)
  download.file(url = "https://datos.cdmx.gob.mx/api/datasets/1.0/base-covid-sinave/attachments/diccionario_xlsx/"
                , destfile = "./data/diccionario_sinave.xlsx")
}

#' Downloads covid national cases from the CDMX site
#'
#' @export
#' @param numRows Numeric. Number of rows to download. Defaults to -1 for all
#' @importFrom RCurl getURL
#' @examples
#' downloadCovidNacionalFromCDMX(2)
downloadCovidNacionalFromCDMX <- function(numRows = -1) {
  dir.create(file.path("./", "data"), showWarnings = FALSE)
  covid = getURL(paste0(
    "https://datos.cdmx.gob.mx/api/v2/catalog/datasets/casos-asociados-a-covid-19/exports/csv?"
    ,"rows=",numRows
    ,"&select=exclude(geo_shape)&timezone=UTC&delimiter=%3B"))
  write.table(x = covid, file = './data/casos-asociados-a-covid-19.csv', row.names = FALSE, sep = ";", col.names = FALSE, quote = FALSE)
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
data.frame.covidmx <- function() {
    datosFilename = list.files("./data", recursive = TRUE, pattern = ".*COVID19MEXICO.csv", full.names = TRUE)
    datosFilename = max(datosFilename)
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
               , MUNICIPIO_RES  = sprintf(MUNICIPIO_RES, fmt = "%03d")
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

#' #' Loads geography of municipios
#' #'
#' #' @export
#' #' @importFrom rgdal readOGR
#' #' @examples
#' #' loadMunicipios()
#' loadMunicipios <- function() {
#'     # data("municipiosmx.geojson")
#'     # municipiosmx <<- readOGR(dsn = municipiosmx)
#'     data("municipiosmx.sp")
#'   }

#' Creates data frames for timeseries treatment of a mexican state. 
#' 
#' The data frame contains FECHA_INGRESO, n = number of cases, cumsum = cumulative sum
#' 
#' @export
#' @importFrom dplyr filter group_by tally mutate order_by
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @examples
#' ## Print plots for all states
#' data('COVID19MEXICO_EXAMPLE')
#' d = COVID19MEXICO_EXAMPLE
#' covid19mx::varDicHealth()
#' 
#' rm(list=grep(pattern = "timeline_estado_.*", ls(), value = TRUE))
#' estados = unique(d$ENTIDAD_RES)
#' sapply(1:length(estados), timelineByState)
#' 
#' timelines = grep(pattern = "timeline_estado_.*", ls(), value = TRUE)
#' sapply(timelines, function(tl){
#'   main = gsub(pattern = "timeline_estado_", x = tl, replacement = "")
#'   main = gsub(pattern = "_fecha_ingreso", x = main, replacement = ""  )
#'   main = gsub(pattern = "_", x = main, replacement = " "  )
#'   main = paste0(main,' - Positivo SARS-CoV-2')
#'   print(
#'   ggplot2::ggplot(data = get(tl), ggplot2::aes(x = FECHA_INGRESO,y = cumsum)) + 
#'    ggplot2::geom_line() +
#'    ggplot2::ggtitle(main) +
#'    ggplot2::ylab('Casos acumulados') +
#'    ggplot2::xlab('Fecha de ingreso a la unidad de atención')
#'    )
#'    })
timelineByState <- function(stateId) {
    edo = gsub(pattern = " ", replacement = '_'
               , x = as.character(Catálogo_de_ENTIDADES [ Catálogo_de_ENTIDADES$CLAVE_ENTIDAD == stateId, 'ENTIDAD_FEDERATIVA']))
    name = paste0('timeline_estado_'
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


#' Creates data frames for timeseries treatment of mexican municipios. 
#' 
#' The data frame contains FECHA_INGRESO, n = number of cases, cumsum = cumulative sum
#' for covid positive cases
#' 
#' @export
#' @importFrom dplyr filter group_by tally mutate order_by
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @examples
#' ## Print plots for all states
#' data('COVID19MEXICO_EXAMPLE')
#' d = COVID19MEXICO_EXAMPLE
#' covid19mx::varDicHealth()
#' 
#' rm(list=grep(pattern = "timeline_municipio_.*", ls(), value = TRUE))
#' municipiosCDMX = unique(d [ d$ENTIDAD_RES == "20", c("ENTIDAD_RES","MUNICIPIO_RES")])
#' sapply(1:nrow(municipiosCDMX), function(r){ 
#' covid19mx::timelineByMunicipio(municipiosCDMX[r,"ENTIDAD_RES"]
#'                               , municipiosCDMX[r,"MUNICIPIO_RES"])
#'                               })
#' 
#' timelines = grep(pattern = "tlm_.*", ls(), value = TRUE)
#' sapply(timelines, function(tl){
#'   main = gsub(pattern = "tlm_", x = tl, replacement = "")
#'   main = gsub(pattern = "_fi", x = main, replacement = ""  )
#'   main = gsub(pattern = "_", x = main, replacement = " "  )
#'   main = paste0(main,' - Positivo SARS-CoV-2')
#'   print(
#'   ggplot2::ggplot(data = get(tl), ggplot2::aes(x = FECHA_INGRESO,y = cumsum)) + 
#'    ggplot2::geom_line() +
#'    ggplot2::ggtitle(main) +
#'    ggplot2::ylab('Casos acumulados') +
#'    ggplot2::xlab('Fecha de ingreso a la unidad de atención')
#'    )
#'    })
timelineByMunicipio <- function(stateId, municipioId) {
    edo = substr(gsub(pattern = " ", replacement = '_'
                      , x = as.character(Catálogo_de_ENTIDADES [ Catálogo_de_ENTIDADES$CLAVE_ENTIDAD == stateId, 'ENTIDAD_FEDERATIVA']))
               , start = 1 , stop = 20)
    mpio = substr(gsub(pattern = " ", replacement = '_'
                       , x = as.character(Catálogo_MUNICIPIOS [ Catálogo_MUNICIPIOS$CLAVE_MUNICIPIO == municipioId &
                                                                  Catálogo_MUNICIPIOS$CLAVE_ENTIDAD == stateId
                                                                , 'MUNICIPIO']))
                  , start = 1, stop = 20)
    name = paste0('tlm_'
                  , mpio
                  , '_'
                  , edo
                  , "_fi")
    d2 = d %>%
      filter(ENTIDAD_RES==stateId) %>%
      filter(MUNICIPIO_RES==municipioId) %>%
      filter(RESULTADO==1) %>% # Positivo SARS-CoV-2
      group_by(FECHA_INGRESO) %>%
      tally() %>%
      mutate(cumsum = order_by(FECHA_INGRESO, cumsum(n)))
    assign(name, d2, envir = globalenv())
  }


#' Downloads Inventario de Programas y Acciones Sociales ante el COVID-19
#' 
#' https://datos.cdmx.gob.mx/explore/dataset/inventario-de-acciones-y-programas-sociales-rendicion-de-cuentas/information/
#' 
#' Fields:
#' nombre_programa_accion social
#' nombre_programa_accion_social
#' descripcion_programa_accion_social
#' tipo_apoyo_unidad
#' fecha_anuncio
#' unidad_responsable
#' categoria
#' fecha_publicacion_gaceta
#' enlace_a_gaceta
#' presupuesto_mxn
#' beneficiarios_poblacion_objetivo
#' metas_fisicas
#' metas_fisicas_unidad
#'
#' @export
#' @param numRows Numeric. Number of rows to download. Defaults to -1 for all
#' @importFrom RCurl getURL
#' @example 
#' downloadIPAS(2)
downloadIPAS <- function(numRows = -1) {
  dir.create(file.path("./", "data"), showWarnings = FALSE)
  sinave = getURL(paste0(
    "https://datos.cdmx.gob.mx/api/v2/catalog/datasets/"
    ,"inventario-de-acciones-y-programas-sociales-rendicion-de-cuentas/exports/csv?"
    ,"rows=",numRows,
    ,"select=exclude(geo_shape)"
    ,"&timezone=UTC&"
    ,"delimiter=%3B"
    ))
  write.table(x = sinave
              , file = './data/inventario-de-acciones-y-programas-sociales-rendicion-de-cuentas.csv'
              , row.names = FALSE
              , sep = ";"
              , col.names = FALSE
              , quote = FALSE)
}

#' Preprocess Inventario de Programas y Acciones Sociales ante el COVID-19
#'
#' @export
#' @importFrom utils read.table
preprocessIPAS <- function() {
  d=read.table(
    './data/inventario-de-acciones-y-programas-sociales-rendicion-de-cuentas.csv', sep=';', header = TRUE)
  d = transform(
    d
    , fecha_anuncio = as.Date(as.character(d$fecha_anuncio))
    , fecha_publicacion_gaceta = 
      as.Date(as.character(d$fecha_publicacion_gaceta))
    , metas_fisicas = 
      as.numeric(gsub(pattern = ','
                      , x = d$metas_fisicas
                      , replacement = ""))
  )
  inventario_de_acciones_y_programas_sociales_rendicion_de_cuentas = d
  save(inventario_de_acciones_y_programas_sociales_rendicion_de_cuentas
       ,file = './data/inventario_de_acciones_y_programas_sociales_rendicion_de_cuentas.rda')
}
