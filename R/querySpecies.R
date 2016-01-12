#' Query tables by species.
#'
#' Search for species records in Invaders, Footprints or Specimens tables, the
#' results are indexed by conglomerate-date and may be filtered by state,
#' organization, date, or conglomerate ids.
#' @param noms String indicating the species names to look for (ex:
#' "vaca|equus")
#' @inheritParams queryCgls
#' @return A \code{data.frame} where each line corresponds to a
#' conglomerate-date, filtered by \code{state}, \code{organization},
#' \code{cgl_id}, \code{year}, and \code{month}, the \code{data.frame}
#' includes conglomerate name, coordinates of the conglomerate center,
#' date of visit, organization, state, municipality, monitoring type,
#' vegetation, perturbated, species common and scientific name, ....
#' @name querySpecies
NULL
#> NULL
#' @examples
#'
#' # connect to sqlite database (snmb)
#' database <- dplyr::src_sqlite(system.file("extdata", "snmb.sqlite",
#'   package = "querysnmb"))
#' invaders <- queryInvaders(database)
#' specimens <- querySpecimens(database, state = "Chiapas")
#'
#' \dontrun{
#' connect to database (snmb)
#' PASS_SNMB = Sys.getenv("PASS_SNMB")
#' database <- dplyr::src_postgres(dbname = "snmb", host = "dbms", user =
#'   "snmb", password = PASS_SNMB)
#' invaders <- queryInvaders(database, noms = "vaca|bos|perro",
#'   year_visit = 2014:2015, organization = c("CONANP", "FMCN"))
#' footprints <- queryFootprints(database, noms = "vaca|bos|equus|caballo")
#' }

#' @rdname querySpecies
#' @export
queryInvaders <- function(database, state = "all", organization = "all",
  cgl_id = "all", year_visit = 2010:2016, month_visit = 1:12, noms = "all") {
  ruta_archivos_cluster <- ifelse(.Platform$OS.type == "unix",
    "/Volumes/sacmod/archivos_snmb/",
    "//madmexservices.conabio.gob.mx/sacmod/archivos_snmb/")

  # base table is computed with the queryCgls function
  cgl_table <- queryCgls(database, state, organization, cgl_id,
    year_visit, month_visit)
  if(nrow(cgl_table) == 0){
    # RPostgreSQL::dbDisconnect(database$con)
    print("No hay registros que cumplan los requisitos solicitados.")
    final_table <- "No hay registros que cumplan los requisitos solicitados."
  }else{
    if(noms == "all"){
      ei_aux <- dplyr::tbl(database, "especie_invasora") %>%
        dplyr::collect() %>%
        dplyr::select(nombre_comun, nombre_cientifico)
      ei_ex_aux <- dplyr::tbl(database, "especie_invasora_extra") %>%
        dplyr::collect() %>%
        dplyr::select(nombre_comun, nombre_cientifico)
      especies <- unique(c(ei_aux$nombre_comun, ei_aux$nombre_cientifico,
        ei_ex_aux$nombre_comun, ei_ex_aux$nombre_cientifico))
      especies <- especies[!is.na(especies) & especies != "NA"]
      noms <- paste(especies, collapse = "|")
    }

    # registros de especies invasoras correspondientes a transectos
    transecto_especie <- dplyr::collect(dplyr::tbl(database,
        "transecto_especies_invasoras_muestra")) %>%
      dplyr::inner_join(cgl_table, by = c("conglomerado_muestra_id")) %>%
      dplyr::select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita,
        institucion, estado, municipio, monitoreo_tipo, vegetacion_tipo,
        perturbado, transecto_especies_invasoras_id = id, transecto_numero,
        comentario)

    especie <- dplyr::collect(dplyr::tbl(database, "especie_invasora")) %>%
      dplyr::inner_join(transecto_especie,
        by = c("transecto_especies_invasoras_id"))  %>%
      dplyr::filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      dplyr::select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita,
        institucion, estado, municipio, monitoreo_tipo, vegetacion_tipo,
        perturbado, transecto_numero,  comentario, especie_invasora_id = id,
        nombre_en_lista, nombre_comun, nombre_cientifico, numero_individuos)

    archivo_especie_trans <- dplyr::tbl(database,
        "archivo_especie_invasora") %>%
      dplyr::collect() %>%
      dplyr::select(-archivo_nombre_original) %>%
      dplyr::inner_join(especie, by = c("especie_invasora_id")) %>%
      dplyr::mutate(
        esta_dentro_conglomerado = "T",
        tabla = "transecto_especie_invasora",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl, "/",
          substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/especies_invasoras/", nuevo_nombre, sep = "")
      ) %>%
      dplyr::group_by(path_archivos_cluster) %>%
      dplyr::mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

    # registros de especies invasoras correspondientes a registros extra
    especie_extra <- dplyr::collect(dplyr::tbl(database,
      "especie_invasora_extra")) %>%
      dplyr::inner_join(cgl_table, by = c("conglomerado_muestra_id"))  %>%
      dplyr::filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      dplyr::select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita,
        institucion, estado, municipio, monitoreo_tipo, vegetacion_tipo,
        perturbado, comentario, especie_invasora_id = id, nombre_en_lista,
        nombre_comun, nombre_cientifico, numero_individuos,
        esta_dentro_conglomerado)

    archivo_especie_extra <- dplyr::collect(
      dplyr::tbl(database, "archivo_especie_invasora_extra")) %>%
      dplyr::select(id, especie_invasora_id = especie_invasora_extra_id,
        archivo) %>%
      dplyr::inner_join(especie_extra, by = c("especie_invasora_id")) %>%
      dplyr::mutate(
        transecto_numero = NA,
        tabla = "especie_invasora_extra",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl, "/",
          substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/registros_extra/", nuevo_nombre, sep = "")
      ) %>%
      dplyr::group_by(path_archivos_cluster) %>%
      dplyr::mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

    final_table <- rbind(archivo_especie_trans, archivo_especie_extra) %>%
      dplyr::ungroup() %>%
      dplyr::select(id, especie_invasora_id, conglomerado_muestra_id, cgl, lat,
        lon, fecha_visita, institucion, estado, municipio, monitoreo_tipo,
        vegetacion_tipo, perturbado, tabla, transecto_numero, comentario,
        nombre_en_lista, nombre_comun, nombre_cientifico, numero_individuos,
        esta_dentro_conglomerado, path_imagen_cluster, path_formato_cluster)
  }
  final_table
}

#' @rdname querySpecies
#' @export
queryFootprints <- function(database, state = "all", organization = "all",
  cgl_id = "all", year_visit = 2010:2016, month_visit = 1:12, noms = "all") {
  ruta_archivos_cluster <- ifelse(.Platform$OS.type == "unix",
    "/Volumes/sacmod/archivos_snmb/",
    "//madmexservices.conabio.gob.mx/sacmod/archivos_snmb/")

  # base table is computed with the queryCgls function
  cgl_table <- queryCgls(database, state, organization, cgl_id, year_visit,
    month_visit)

  if(nrow(cgl_table) == 0){
    # RPostgreSQL::dbDisconnect(database$con)
    print("No hay registros que cumplan los requisitos solicitados.")
    final_table <- "No hay registros que cumplan los requisitos solicitados."
  }else{
    if(noms == "all"){
      he_aux <- dplyr::tbl(database, "huella_excreta") %>%
        dplyr::collect() %>%
        dplyr::select(nombre_comun, nombre_cientifico)
      he_ex_aux <- dplyr::tbl(database, "huella_excreta_extra") %>%
        dplyr::collect() %>%
        dplyr::select(nombre_comun, nombre_cientifico)
      huellas <- unique(c(he_aux$nombre_comun, he_aux$nombre_cientifico,
        he_ex_aux$nombre_comun, he_ex_aux$nombre_cientifico))
      huellas <- huellas[!is.na(huellas) & huellas != "NA"]
      noms <- paste(huellas, collapse = "|")
    }
    # registros de huellas/excretas correspondientes a transectos
    transecto_huella <- dplyr::collect(dplyr::tbl(database,
        "transecto_huellas_excretas_muestra")) %>%
      dplyr::inner_join(cgl_table, by = c("conglomerado_muestra_id")) %>%
      dplyr::select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita,
        institucion, estado, municipio, monitoreo_tipo, vegetacion_tipo,
        perturbado, transecto_huellas_excretas_id = id, transecto_numero,
        comentario)

    huella <- dplyr::collect(dplyr::tbl(database, "huella_excreta")) %>%
      dplyr::inner_join(transecto_huella,
        by = c("transecto_huellas_excretas_id"))  %>%
      dplyr::filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      dplyr::select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita,
        institucion, estado, municipio, monitoreo_tipo, vegetacion_tipo,
        perturbado, transecto_numero,  comentario, huella_excreta_id = id,
        es_huella, nombre_comun, nombre_cientifico, largo, ancho)

    archivo_huella_trans <- dplyr::collect(dplyr::tbl(database,
      "archivo_huella_excreta")) %>%
      dplyr::select(-archivo_nombre_original) %>%
      dplyr::inner_join(huella, by = c("huella_excreta_id")) %>%
      dplyr::mutate(
        esta_dentro_conglomerado = "T",
        tabla = "transecto_huella_excreta",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl, "/",
          substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/huellas_excretas/", nuevo_nombre, sep = "")
      ) %>%
      dplyr::group_by(path_archivos_cluster) %>%
      dplyr::mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

    # registros de huellas/excretas correspondientes a registros extra
    huella_extra <- dplyr::collect(dplyr::tbl(database,
      "huella_excreta_extra")) %>%
      dplyr::inner_join(cgl_table, by = c("conglomerado_muestra_id"))  %>%
      dplyr::mutate(
        transecto_numero = NA
      ) %>%
      dplyr::filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      dplyr::select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        transecto_numero, comentario, huella_excreta_id = id, es_huella,
        nombre_comun, nombre_cientifico, largo, ancho,
        esta_dentro_conglomerado)

    archivo_huella_extra <- dplyr::collect(
      dplyr::tbl(database, "archivo_huella_excreta_extra")) %>%
      dplyr::select(id, huella_excreta_id = huella_excreta_extra_id, archivo) %>%
      dplyr::inner_join(huella_extra, by = c("huella_excreta_id")) %>%
      dplyr::mutate(
        tabla = "huella_excreta_extra",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl,
          "/", substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/registros_extra/", nuevo_nombre, sep = "")
      ) %>%
      dplyr::group_by(path_archivos_cluster) %>%
      dplyr::mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

    final_table <- dplyr::ungroup(rbind(archivo_huella_trans,
      archivo_huella_extra)) %>%
      dplyr::select(id, huella_excreta_id, conglomerado_muestra_id, cgl, lat, lon,
        fecha_visita, institucion, estado, municipio, monitoreo_tipo,
        vegetacion_tipo, perturbado, tabla, transecto_numero, comentario,
        nombre_comun, nombre_cientifico, es_huella, largo, ancho,
        esta_dentro_conglomerado, path_imagen_cluster, path_formato_cluster)
    }
  final_table
}

#' @rdname querySpecies
#' @export
querySpecimens <- function(database, state = "all", organization = "all",
  cgl_id = "all", year_visit = 2010:2016, month_visit = 1:12, noms = "all") {
  ruta_archivos_cluster <- ifelse(.Platform$OS.type == "unix",
    "/Volumes/sacmod/archivos_snmb/",
    "//madmexservices.conabio.gob.mx/sacmod/archivos_snmb/")

  # base table is computed with the queryCgls function
  cgl_table <- queryCgls(database, state, organization, cgl_id, year_visit,
    month_visit)

  if(nrow(cgl_table) == 0){
    # RPostgreSQL::dbDisconnect(database$con)
    print("No hay registros que cumplan los requisitos solicitados.")
    final_table <- "No hay registros que cumplan los requisitos solicitados."
  }else{
    if(noms == "all"){
      specimen_ex_aux <- dplyr::tbl(database, "especimen_restos_extra") %>%
        dplyr::collect() %>%
        dplyr::select(nombre_comun, nombre_cientifico)
      specimens <- unique(c(specimen_ex_aux$nombre_comun,
        specimen_ex_aux$nombre_cientifico))
      specimens <- specimens[!is.na(specimens) & specimens != "NA"]
      noms <- paste(specimens, collapse = "|")
    }

    # registros de especimen/restos correspondientes a registros extra
    specimen_extra <- dplyr::collect(dplyr::tbl(database, "especimen_restos_extra")) %>%
      dplyr::inner_join(cgl_table, by = c("conglomerado_muestra_id"))  %>%
      dplyr::filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      dplyr::select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        comentario, especimen_restos_extra_id = id,
        es_especimen, nombre_comun, nombre_cientifico, numero_individuos,
        esta_dentro_conglomerado)

    archivo_especimen_extra <- dplyr::collect(
      dplyr::tbl(database, "archivo_especimen_restos_extra")) %>%
      dplyr::select(-archivo_nombre_original) %>%
      dplyr::inner_join(specimen_extra, by = c("especimen_restos_extra_id")) %>%
      dplyr::mutate(
        tabla = "especimen_restos_extra",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl,
          "/", substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/registros_extra/", nuevo_nombre, sep = "")
      ) %>%
      dplyr::group_by(path_archivos_cluster) %>%
      dplyr::mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

      final_table <- dplyr::ungroup(archivo_especimen_extra) %>%
        dplyr::select(id, especimen_restos_extra_id, conglomerado_muestra_id,
          cgl, lat, lon, fecha_visita, institucion, estado, municipio,
          monitoreo_tipo, vegetacion_tipo, perturbado, tabla, comentario,
          nombre_comun, nombre_cientifico, es_especimen, numero_individuos,
          esta_dentro_conglomerado, path_imagen_cluster, path_formato_cluster)
    }
  final_table
}


