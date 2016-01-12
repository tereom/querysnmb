#' Paths to recordings or images
#'
#' \code{queryFiles} returns paths to recordings, images or videos. For this
#' function to work you need to be \strong{connected to madmex folder}, this is
#' because the function searches for existing files in the madmex directories.
#' @param database  Connection to an existing postgresql or sqlite database (as
#'   returned by \code{\link[dplyr]{src_postgres}} or
#'   \code{\link[dplyr]{src_sqlite}}.
#' @param file_type String specifing which file paths should be retrieved, value
#'   should be "recordings" or "images" (includes images and videos from the
#'   camera trap).
#' @param photo,video,ultrasonic Binary variables, if \code{file_type = "images"},
#'   \code{photo = FALSE, video = TRUE} indicates to only search for videos,
#'   similarly \code{video = FALSE, photo = TRUE excludes videos from results,
#'   defult sets both to \code{TRUE}. In case of recordings it is only possible
#'   to download one type of file (acoustic OR ultrasonic), defaults to download
#'   acoustic, this is \code{ultrasonic = FALSE}.
#' @param organization Character vector of organizations to include in the
#'   search (CONAFOR, CONANP, FMCN), defaults to "all".
#' @param cgl_id Character vector of conglomerate ids to include in the
#'   searh, defaults to "all".
#' @param year_visit Numeric vector indicating the year(s) to include in the
#'   search, defaults to 2014:2016.
#' @param month_visit Numeric vector indicating the month(s) to include in the
#'    search, defaults to 1:12.
#' @param original Indicates if a column with the original file name should be
#'    included in the results.
#' @return A \code{data.frame} containing the following columns:
#'    \code{file_type}, \code{state}, \code{institucion} (organization),
#'    \code{cgl_id} (conglomerate id), \code{date} (date of visit),
#'    \code{lat, lon} (coordinates of the conglomerate), and
#'    \code{archivo_nombre_original} (if original = TRUE, the original file
#'    name).
#'
#' @examples
#' # connect to sqlite database (snmb)
#' database <- dplyr::src_sqlite(system.file("extdata", "snmb.sqlite",
#'   package = "querysnmb"))
#' files_images_videos <- queryFiles(database, file_type = "images",
#'   cgl_id = "405", year_visit = 2014)
#' files_videos <- queryFiles(database, file_type = "images", cgl_id = "2286",
#'   year_visit = 2014, photo = FALSE)
#' files_acoustic <- queryFiles(database, file_type = "recordings",
#'   ultrasonic = FALSE, year_visit = 2014)
#' \dontrun{
#' # connect to postgreSQL database (snmb)
#' PASS_SNMB = Sys.getenv("PASS_SNMB")
#' database <- dplyr::src_postgres(dbname = "snmb", host = "dbms", user =
#'   "snmb", password = PASS_SNMB)
#' files_acoustic_name <- queryFiles(file_type = "recordings",
#'   ultrasonic = FALSE, year_visit = 2014, original = TRUE)
#' }
#'
#' @importFrom magrittr %>%
#' @export
queryFiles <- function(database, file_type, state = "all", organization = "all",
  cgl_id = "all", year_visit = 2010:2016, month_visit = 1:12, photo = TRUE,
  video = TRUE, ultrasonic = FALSE, original = FALSE){

  # el método actual puede ocasionar problemas en el caso original = TRUE si hay
  # dos archivos con el mismo nombre los resultados fallarán.
  # Se puede mejorar el código haciendo lazy evaluation y ejecutamos los queries
  # hasta que filtramos.
  # connect to database (snmb)

  cgl_table_aux <- dplyr::tbl(database, "conglomerado_muestra") %>%
    dplyr::collect()

  # set states and organization to valid values
  if(state == "all"){
    state <- unique(cgl_table_aux$estado)
  }
  if(organization == "all"){
    organization <- unique(cgl_table_aux$institucion)
  }
  if(cgl_id == "all"){
    cgl_id <- unique(cgl_table_aux$nombre)
  }else if(class(cgl_id) == "numeric"){
    cgl_id <- as.character(cgl_id)
  }
  # filter according to function arguments
  cgl_table <- cgl_table_aux %>%
    dplyr::mutate(
      anio = lubridate::year(fecha_visita),
      mes = lubridate::month(fecha_visita)
    ) %>%
    dplyr::filter(estado %in% state, institucion %in% organization,
      nombre %in% cgl_id, anio %in% year_visit, mes %in% month_visit) %>%
    dplyr:: select(conglomerado_muestra_id = id, estado, institucion,
      cgl_id = nombre, fecha_visita, anio, mes)

  if(nrow(cgl_table) == 0){
    print("No hay registros que cumplan los requisitos solicitados.")
    final_table <- NA
  }else{

    if(original){
      cgl_paths_table <- searchPathsOriginal(database, cgl_table, file_type,
        photo, video, ultrasonic)
    }else{
      cgl_paths_table <- searchPaths(database, cgl_table, file_type, photo,
        video, ultrasonic)
    }
    if(nrow(cgl_paths_table) == 0){
      print("No hay achivos que cumplan los requisitos solicitados.")
      final_table <- NA
    }else{
      final_table <- dplyr::collect(dplyr::tbl(database, "sitio_muestra")) %>%
        dplyr::filter(sitio_numero == "Centro") %>%
        dplyr::right_join(cgl_paths_table, by = "conglomerado_muestra_id") %>%
        dplyr::mutate(
          file_type = file_type,
          lat = lat_grado + lat_min / 60 + lat_seg / 3600,
          lon = ifelse(lon_grado > 0,  lon_grado + lon_min/60 + lon_seg/3600,
            -(lon_grado - lon_min/60 - lon_seg/3600))
        )
      if(original){
        final_table <- final_table %>%
          dplyr::select(file_type, estado, institucion, cgl_id, fecha_visita,
            lat, lon, path, archivo_nombre_original)
      }else{
        final_table <- final_table %>%
          dplyr::select(file_type, estado, institucion, cgl_id, fecha_visita,
            lat, lon, path)
      }
    }
  }
  final_table
}

searchPaths <- function(database, cgl_table, file_type, photo = TRUE,
  video = TRUE, ultrasonic = FALSE){
  # returns a data.frame of path per cgl_id, year_visit, month
  # we create paths according to the directory structure of media

  ruta_archivos_cluster <- ifelse(.Platform$OS.type == "unix",
    "/Volumes/sacmod/archivos_snmb/",
    "//madmexservices.conabio.gob.mx/sacmod/archivos_snmb/")

  # create a list of potential directory paths using cross product
  mes_aux <- paste("0", cgl_table$mes, sep = "")
  mes <- substr(mes_aux, start = nchar(mes_aux) - 1, stop = nchar(mes_aux))
  expr_aux <- paste(cgl_table$anio, mes, sep = "_")
  cgl_table$expr <- paste(cgl_table$cgl_id, expr_aux, sep = "/")

  # select images or recodings directories
  if(file_type == "recordings"){
    folder <- ifelse(ultrasonic, "grabaciones_ultrasonicas",
      "grabaciones_audibles")
    pattern <- "."
  }else{
    folder <- "fotos_videos"
    pattern <- ifelse(photo & video, ".", ifelse(video, "\\.AVI$", "\\.JPG$"))
  }
  # find directories
  dirs_search <- paste(ruta_archivos_cluster, cgl_table$expr, "/", folder,
    sep = "")
  # find files in the existing directories
  files_search <- list.files(dirs_search, full.names = TRUE, pattern = pattern)
  # create a data. frame with the files's path
  paths_df <- data.frame(
    expr = stringi::stri_extract_first(str = files_search,
      regex = "[:digit:]+/[:digit:]+_[:digit:]+"), path = files_search,
    stringsAsFactors = FALSE)
  cgl_paths <- dplyr::left_join(paths_df, cgl_table, by = "expr")

  cgl_paths
}


searchPathsOriginal <- function(database, cgl_table, file_type, photo = TRUE,
  video = TRUE, ultrasonic = FALSE){
  # returns a data.frame of path per cgl_id, year_visit, month
  # we create paths according to the directory structure of media
  # create a list of potential directory paths using cross product

  ruta_archivos_cluster <- ifelse(.Platform$OS.type == "unix",
    "/Volumes/sacmod/archivos_snmb/",
    "//madmexservices.conabio.gob.mx/sacmod/archivos_snmb/")

  mes_aux <- paste("0", cgl_table$mes, sep = "")
  mes <- substr(mes_aux, start = nchar(mes_aux) - 1, stop = nchar(mes_aux))
  expr_aux <- paste(cgl_table$anio, mes, sep = "_")
  cgl_table$expr <- paste(cgl_table$cgl_id, expr_aux, sep = "/")

  # select images or recodings directories
  if(file_type == "recordings"){
    folder <- ifelse(ultrasonic, "grabaciones_ultrasonicas",
      "grabaciones_audibles")
    pattern <- "."
    tab <- "archivo_grabadora"
  }else{
    folder <- "fotos_videos"
    pattern <- ifelse(photo & video, ".", ifelse(video, "\\.AVI$", "\\.JPG$"))
    tab <- "archivo_camara"
  }
  # find directories
  dirs_search <- paste(ruta_archivos_cluster, cgl_table$expr, "/", folder,
    sep = "")
  # find files in the existing directories
  files_search <- list.files(dirs_search, full.names = TRUE, pattern = pattern)
  # create a data. frame with the files's path
  paths_df <- data.frame(
    expr = stringi::stri_extract_first(str = files_search,
      regex = "[:digit:]+/[:digit:]+_[:digit:]+"), path = files_search,
      stringsAsFactors = FALSE)
  cgl_paths <-
    dplyr::left_join(paths_df, cgl_table, by = "expr") %>%
    dplyr::mutate(
      archivo_nombre_aux = basename(path)
    )
  if(file_type != "recordings"){
  cgl_paths <- cgl_paths %>%
    dplyr::mutate(
      archivo_nombre_aux = substr(archivo_nombre_aux, start = 4,
        stop = nchar(archivo_nombre_aux))
    )
  }
  tab <- dplyr::tbl(database, tab) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      archivo_nombre_aux = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo)
    ) %>%
    dplyr::select(archivo_nombre_aux, archivo_nombre_original)
  cgl_name <- cgl_paths %>%
    dplyr::left_join(tab, by = "archivo_nombre_aux") %>%
    dplyr::select(-archivo_nombre_aux)
  cgl_name
}


