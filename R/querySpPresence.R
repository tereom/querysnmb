#' Tables of species presence by conglomerate.
#'
#' Search for species records in Invaders, Footprints, Specimens and Camera
#' tables.
#' @param noms String indicating the species names to look for (ex:
#' 'vaca|equus')
#' @param database  Connection to an existing postgresql or sqlite database (as
#'   returned by \code{\link[dplyr]{src_postgres}} or
#'   \code{\link[dplyr]{src_sqlite}}.
#' @param malla Optional parameter indicating the coordinates of the
#'   conglomerates, malla most be a data.frame with columns cgl_id, lng, lat,
#'   where cgl_id is the conglomerate id (class double), lng is longitude and
#'   lat is latitude. If \code{malla} is left to NULL the function will return
#'   the coordinates from the conglomerate Center specified in the database.
#' @return A list with 2 elements (both have class \code{data.frame}):
#'   \enumerate{
#'     \item The first element is indexed by conglomerate id (\emph{Id}) and
#'      includes only the conglomerates where the species specified by noms were
#'      found, its columns are: \emph{Estado}, \emph{Municipio}, and
#'      \emph{Uso suelo}, it also includes an indicator of
#'      the kind of records found, this is, if records belong to invaders
#'      (\emph{EI}), footprints/feces (\emph{HE}), opportunistic invaders
#'      (\emph{EI ex}), opportunistic footprints/feces, opportunistic
#'      specimens/remains (\emph{ER ex}) or camera trap (\emph{Cámara}).
#'    \item The second element is indexed by conglomerate id (\emph{Id}) and
#'      includes only the conglomerates where the species specified by noms were
#'      found, the columns are the coordinates of the conglomerate
#'      (\emph{lng, lat}), the coordinates can be supplied by the user (throught
#'      the parameter \code{malla}) or taken from the database.
#'    }
#'
#' @examples
#' # connect to sqlite database (snmb)
#' database <- dplyr::src_sqlite(system.file('extdata', 'snmb.sqlite',
#'   package = 'querysnmb'))
#' presence_cows <- querySpPresence(database, noms = 'vaca|bos|taurus')
#' \dontrun{
#' # connect to postgreSQL database (snmb)
#' PASS_SNMB = Sys.getenv('PASS_SNMB')
#' database <- dplyr::src_postgres(dbname = 'snmb', host = 'dbms', user =
#'   'snmb', password = PASS_SNMB)
#' presence_cows <- querySpPresence(database, noms = 'vaca|bos|taurus')
#' }

#' @importFrom magrittr %>%
#' @export
querySpPresence <- function(database, noms, malla = NULL) {
    conglomerado <- dplyr::tbl(database, "conglomerado_muestra") %>% dplyr::collect() %>% 
        dplyr::select(conglomerado_muestra_id = id, nombre, estado, municipio, uso = uso_suelo_tipo)
    
    if (is.null(malla)) {
        malla <- dplyr::collect(dplyr::tbl(database, "sitio_muestra")) %>% dplyr::filter(sitio_numero == 
            "Centro") %>% dplyr::mutate(lat = lat_grado + lat_min/60 + lat_seg/3600, 
            lng = ifelse(lon_grado > 0, lon_grado + lon_min/60 + lon_seg/3600, -(lon_grado - 
                lon_min/60 - lon_seg/3600)), lng = -lng) %>% dplyr::right_join(conglomerado, 
            by = c("conglomerado_muestra_id")) %>% dplyr::mutate(cgl_id = as.numeric(nombre)) %>% 
            dplyr::select(cgl_id, lng, lat)
    }
    
    sitio <- dplyr::tbl(database, "sitio_muestra") %>% dplyr::collect() %>% dplyr::select(conglomerado_muestra_id, 
        sitio_muestra_id = id) %>% dplyr::inner_join(conglomerado, by = "conglomerado_muestra_id") %>% 
        dplyr::select(sitio_muestra_id, conglomerado_muestra_id, nombre)
    
    tr_ei <- dplyr::tbl(database, "transecto_especies_invasoras_muestra") %>% dplyr::collect() %>% 
        dplyr::select(transecto_especies_invasoras_id = id, conglomerado_muestra_id) %>% 
        dplyr::left_join(conglomerado, by = "conglomerado_muestra_id")
    
    ei <- dplyr::tbl(database, "especie_invasora") %>% dplyr::collect() %>% dplyr::mutate(ei_esp = grepl(noms, 
        nombre_comun, ignore.case = TRUE) | grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>% 
        dplyr::select(transecto_especies_invasoras_id, ei_esp) %>% dplyr::left_join(tr_ei, 
        by = "transecto_especies_invasoras_id") %>% dplyr::group_by(nombre) %>% dplyr::summarise(ei_esp = sum(ei_esp, 
        na.rm = TRUE)) %>% dplyr::select(nombre, ei_esp)
    
    ei_ex <- dplyr::tbl(database, "especie_invasora_extra") %>% dplyr::collect() %>% 
        dplyr::mutate(ei_ex_esp = grepl(noms, nombre_comun, ignore.case = TRUE) | grepl(noms, 
            nombre_cientifico, ignore.case = TRUE)) %>% dplyr::select(conglomerado_muestra_id, 
        ei_ex_esp) %>% dplyr::right_join(conglomerado, by = "conglomerado_muestra_id") %>% 
        dplyr::group_by(nombre) %>% dplyr::summarise(ei_ex_esp = sum(ei_ex_esp, na.rm = TRUE))
    
    er_ex <- dplyr::tbl(database, "especimen_restos_extra") %>% dplyr::collect() %>% 
        dplyr::mutate(er_ex_esp = grepl(noms, nombre_comun, ignore.case = TRUE) | grepl(noms, 
            nombre_cientifico, ignore.case = TRUE)) %>% dplyr::select(conglomerado_muestra_id, 
        er_ex_esp) %>% dplyr::right_join(conglomerado, by = "conglomerado_muestra_id") %>% 
        dplyr::group_by(nombre) %>% dplyr::summarise(er_ex_esp = sum(er_ex_esp, na.rm = TRUE))
    
    tr_he <- dplyr::tbl(database, "transecto_huellas_excretas_muestra") %>% dplyr::collect() %>% 
        dplyr::select(transecto_huellas_excretas_id = id, conglomerado_muestra_id) %>% 
        dplyr::left_join(conglomerado, by = "conglomerado_muestra_id")
    
    he <- dplyr::tbl(database, "huella_excreta") %>% dplyr::collect() %>% dplyr::mutate(he_esp = grepl(noms, 
        nombre_comun, ignore.case = TRUE) | grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>% 
        dplyr::select(transecto_huellas_excretas_id, he_esp) %>% dplyr::left_join(tr_he, 
        by = "transecto_huellas_excretas_id") %>% dplyr::group_by(nombre) %>% dplyr::summarise(he_esp = sum(he_esp, 
        na.rm = TRUE)) %>% dplyr::select(nombre, he_esp)
    
    he_ex <- dplyr::tbl(database, "huella_excreta_extra") %>% dplyr::collect() %>% 
        dplyr::mutate(he_ex_esp = grepl(noms, nombre_comun, ignore.case = TRUE) | grepl(noms, 
            nombre_cientifico, ignore.case = TRUE)) %>% dplyr::select(conglomerado_muestra_id, 
        he_ex_esp) %>% dplyr::right_join(conglomerado, by = "conglomerado_muestra_id") %>% 
        dplyr::group_by(nombre) %>% dplyr::summarise(he_ex_esp = sum(he_ex_esp, na.rm = TRUE))
    
    camara <- dplyr::tbl(database, "camara") %>% dplyr::collect() %>% dplyr::select(camara_id = id, 
        sitio_muestra_id) %>% dplyr::left_join(sitio, by = "sitio_muestra_id")
    
    ar_camara <- dplyr::tbl(database, "archivo_camara") %>% dplyr::collect() %>% dplyr::mutate(camara_esp = grepl(noms, 
        nombre_comun, ignore.case = TRUE) | grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>% 
        dplyr::select(camara_id, camara_esp) %>% dplyr::left_join(camara, by = "camara_id") %>% 
        dplyr::group_by(nombre) %>% dplyr::summarise(camara_esp = sum(camara_esp, na.rm = TRUE))
    
    desagregado <- conglomerado %>% dplyr::left_join(ei, by = "nombre") %>% dplyr::left_join(he, 
        by = "nombre") %>% dplyr::left_join(ei_ex, by = "nombre") %>% dplyr::left_join(he_ex, 
        by = "nombre") %>% dplyr::left_join(er_ex, by = "nombre") %>% dplyr::left_join(ar_camara, 
        by = "nombre") %>% dplyr::mutate_each(dplyr::funs(naZero), contains("esp")) %>% 
        dplyr::mutate(pres = (ei_esp + he_esp + ei_ex_esp + he_ex_esp + er_ex_esp + 
            camara_esp) > 0, cgl_id = as.numeric(nombre)) %>% dplyr::filter(pres) %>% 
        # solo imprimir cgls que tienen coordenadas asignadas
    dplyr::inner_join(malla, by = "cgl_id") %>% dplyr::arrange(cgl_id)
    presencia <- desagregado %>% dplyr::select(cgl_id, lng, lat)
    desagregado_limpio <- desagregado %>% dplyr::select(-conglomerado_muestra_id, -cgl_id, 
        -lng, -lat, -pres) %>% dplyr::mutate_each(dplyr::funs(codeAsChar), contains("esp"))
    colnames(desagregado_limpio) = c("Id", "Estado", "Municipio", "Uso suelo", "EI", 
        "HE", "EI ex", "HE ex", "ER ex", "Cámara")
    list(desagregado = desagregado_limpio, presencia = presencia)
}
