
#' Basic table by conglomerate-date.
#'
#' \code{queryCgls} creates basic tables by conglomerate-date.
#' @param database  Connection to an existing postgresql database (as returned
#'   by src_postgresql.
#' @param state Character vector of Mexican states to include in the searh,
#'   defaults to "all".
#' @param organization Character vector of organizations to include in the
#'   search (CONAFOR, CONANP, FMCN), defaults to "all".
#' @param cgl_id A number.Character vector of conglomerate ids to include in the
#'   searh, defaults to "all".
#' @param year_visit Numeric vector indicating the year(s) to include in the
#'   search, defaults to 2014:2016.
#' @param year_visit Numeric vector indicating the month(s) to include in the
#'    search, defaults to 1:12.
#' @return A \code{data.frame} where each line corresponds to a
#'   conglomerate-date, filtered by \code{state}, \code{organization},
#'   \code{cgl_id}, \code{year}, and \code{month}, the \code{data.frame}
#'   includes conglomerate name, coordinates of the conglomerate center, date of
#'   visit, organization, state, municipality, monitoring type, vegetation, and
#'   perturbated.
#' @examples
#'
#' \dontrun{
#' connect to sqlite database (snmb)
#' database <- dplyr::src_sqlite(system.file("extdata", "snmb.sqlite", package = "querysnmb"))
#' cgl_table <- queryCgls(database)
#' cgl_table <- queryCgls(database, organization = "CONAFOR", year = 2014)
#' }
#'
#' \dontrun{
#' # connect to database (snmb)
#' PASS_SNMB = Sys.getenv("PASS_SNMB")
#' database <- dplyr::src_postgres(dbname = "snmb", host = "dbms", user =
#' "snmb", password = PASS_SNMB)
#' cgl_table <- queryCgls(database)
#' cgl_table <- queryCgls(database, organization = "CONAFOR", year = 2014)
#' }
#'
#'@importFrom magrittr %>%
#' @export
queryCgls <- function(database, state = "all", organization = "all",
  cgl_id = "all", year_visit = 2010:2016, month_visit = 1:12) {

  cgl_table_aux <- dplyr::tbl(database, "conglomerado_muestra") %>%
    dplyr::collect()
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
  cgl_table_filters <- cgl_table_aux %>%
    dplyr::mutate(
      anio = lubridate::year(fecha_visita),
      mes = lubridate::month(fecha_visita)
    ) %>%
    dplyr::filter(estado %in% state, institucion %in% organization,
      nombre %in% cgl_id, anio %in% year_visit, mes %in% month_visit) %>%
    dplyr::select(conglomerado_muestra_id = id, cgl = nombre, institucion,
      fecha_visita, estado, municipio, monitoreo_tipo, vegetacion_tipo,
      perturbado)
  cgl_table <- dplyr::collect(dplyr::tbl(database, "sitio_muestra")) %>%
    dplyr::filter(sitio_numero == "Centro") %>%
    dplyr::inner_join(cgl_table_filters, by = "conglomerado_muestra_id") %>%
    dplyr::mutate(
      lat = lat_grado + lat_min/60 + lat_seg/3600,
      lon = ifelse(lon_grado > 0,  lon_grado + lon_min/60 + lon_seg/3600,
        -(lon_grado - lon_min/60 - lon_seg/3600)),
      lon = -lon
    ) %>%
    dplyr::select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita,
      institucion, estado, municipio, monitoreo_tipo, vegetacion_tipo,
      perturbado)

}
