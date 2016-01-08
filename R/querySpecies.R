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
#' @param x,y numeric vectors.
#' @name querySpecies
NULL
#> NULL
#' @examples
#' \dontrun{
#' connect to database (snmb)
#' PASS_SNMB = Sys.getenv("PASS_SNMB")
#' database <- dplyr::src_postgres(dbname = "snmb", host = "dbms", user =
#' "snmb", password = PASS_SNMB)
#' invaders <- queryInvaders()
#' footprints <- queryFootprints(noms = "vaca|bos|equus|caballo")
#' \}

#' @rdname querySpecies
queryInvaders <- function(database, state = "all", organization = "all",
  cgl_id = "all", year_visit = 2010:2016, month_visit = 1:12, noms = "all") {
  ruta_archivos_cluster <- ifelse(.Platform$OS.type == "unix",
    "/Volumes/sacmod/archivos_snmb/",
    "//madmexservices.conabio.gob.mx/sacmod/archivos_snmb/")

  # base table is computed with the queryCgls function
  cgl_table <- querysnmb:queryCgls(database, state, organization, cgl_id,
    year_visit, month_visit)
  if(nrow(cgl_table) == 0){
    # RPostgreSQL::dbDisconnect(database$con)
    print("No hay registros que cumplan los requisitos solicitados.")
    final_table <- "No hay registros que cumplan los requisitos solicitados."
  }else{
    if(noms == "all"){
      ei_aux <- dplyr::tbl(database, "especie_invasora") %>%
        collect() %>%
        select(nombre_comun, nombre_cientifico)
      ei_ex_aux <- tbl(database, "especie_invasora_extra") %>%
        collect() %>%
        select(nombre_comun, nombre_cientifico)
      especies <- unique(c(ei_aux$nombre_comun, ei_aux$nombre_cientifico,
        ei_ex_aux$nombre_comun, ei_ex_aux$nombre_cientifico))
      especies <- especies[!is.na(especies) & especies != "NA"]
      noms <- paste(especies, collapse = "|")
    }

    # registros de especies invasoras correspondientes a transectos
    transecto_especie <- dplyr::collect(dplyr::tbl(database,
        "transecto_especies_invasoras_muestra")) %>%
      inner_join(cgl_table, by = c("conglomerado_muestra_id")) %>%
      select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        transecto_especies_invasoras_id = id, transecto_numero, comentario)

    especie <- collect(tbl(database, "especie_invasora")) %>%
      inner_join(transecto_especie,
        by = c("transecto_especies_invasoras_id"))  %>%
      filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        transecto_numero,  comentario, especie_invasora_id = id,
        nombre_en_lista, nombre_comun, nombre_cientifico, numero_individuos)

    archivo_especie_trans <- tbl(database, "archivo_especie_invasora") %>%
      collect() %>%
      select(-archivo_nombre_original) %>%
      inner_join(especie, by = c("especie_invasora_id")) %>%
      mutate(
        esta_dentro_conglomerado = "T",
        tabla = "transecto_especie_invasora",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl, "/",
          substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/especies_invasoras/", nuevo_nombre, sep = "")
      ) %>%
      group_by(path_archivos_cluster) %>%
      mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

    # registros de especies invasoras correspondientes a registros extra
    especie_extra <- collect(tbl(database, "especie_invasora_extra")) %>%
      inner_join(cgl_table, by = c("conglomerado_muestra_id"))  %>%
      filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        comentario, especie_invasora_id = id, nombre_en_lista,
        nombre_comun, nombre_cientifico, numero_individuos,
        esta_dentro_conglomerado)

    archivo_especie_extra <- collect(
      tbl(database, "archivo_especie_invasora_extra")) %>%
      select(id, especie_invasora_id = especie_invasora_extra_id, archivo) %>%
      inner_join(especie_extra, by = c("especie_invasora_id")) %>%
      mutate(
        transecto_numero = NA,
        tabla = "especie_invasora_extra",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl, "/",
          substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/registros_extra/", nuevo_nombre, sep = "")
      ) %>%
      group_by(path_archivos_cluster) %>%
      mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

    final_table <- rbind(archivo_especie_trans, archivo_especie_extra) %>%
      ungroup() %>%
      select(id, especie_invasora_id, conglomerado_muestra_id, cgl, lat, lon,
        fecha_visita, institucion, estado, municipio, monitoreo_tipo,
        vegetacion_tipo, perturbado, tabla, transecto_numero, comentario,
        nombre_en_lista, nombre_comun, nombre_cientifico, numero_individuos,
        esta_dentro_conglomerado, path_imagen_cluster, path_formato_cluster)
  }
  final_table
}

#' @rdname querySpecies
queryFootprints <- function(database, state = "all", organization = "all",
  cgl_id = "all", year_visit = 2010:2016, month_visit = 1:12, noms = "all") {

  # base table is computed with the queryCgls function
  cgl_table <- queryCgls(database, state, organization, cgl_id, year_visit,
    month_visit)

  if(nrow(cgl_table) == 0){
    # RPostgreSQL::dbDisconnect(database$con)
    print("No hay registros que cumplan los requisitos solicitados.")
    final_table <- "No hay registros que cumplan los requisitos solicitados."
  }else{
    if(noms == "all"){
      he_aux <- tbl(database, "huella_excreta") %>%
        collect() %>%
        select(nombre_comun, nombre_cientifico)
      he_ex_aux <- tbl(database, "huella_excreta_extra") %>%
        collect() %>%
        select(nombre_comun, nombre_cientifico)
      huellas <- unique(c(he_aux$nombre_comun, he_aux$nombre_cientifico,
        he_ex_aux$nombre_comun, he_ex_aux$nombre_cientifico))
      huellas <- huellas[!is.na(huellas) & huellas != "NA"]
      noms <- paste(huellas, collapse = "|")
    }
    # registros de huellas/excretas correspondientes a transectos
    transecto_huella <- collect(tbl(database,
        "transecto_huellas_excretas_muestra")) %>%
      inner_join(cgl_table, by = c("conglomerado_muestra_id")) %>%
      select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        transecto_huellas_excretas_id = id, transecto_numero, comentario)

    huella <- collect(tbl(database, "huella_excreta")) %>%
      inner_join(transecto_huella,
        by = c("transecto_huellas_excretas_id"))  %>%
      filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        transecto_numero,  comentario, huella_excreta_id = id,
        es_huella, nombre_comun, nombre_cientifico, largo, ancho)

    archivo_huella_trans <- collect(tbl(database, "archivo_huella_excreta")) %>%
      select(-archivo_nombre_original) %>%
      inner_join(huella, by = c("huella_excreta_id")) %>%
      mutate(
        esta_dentro_conglomerado = "T",
        tabla = "transecto_huella_excreta",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl, "/",
          substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/huellas_excretas/", nuevo_nombre, sep = "")
      ) %>%
      group_by(path_archivos_cluster) %>%
      mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

    # registros de huellas/excretas correspondientes a registros extra
    huella_extra <- collect(tbl(database, "huella_excreta_extra")) %>%
      inner_join(cgl_table, by = c("conglomerado_muestra_id"))  %>%
      mutate(
        transecto_numero = NA
      ) %>%
      filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        transecto_numero, comentario, huella_excreta_id = id, es_huella,
        nombre_comun, nombre_cientifico, largo, ancho,
        esta_dentro_conglomerado)

    archivo_huella_extra <- collect(
      tbl(database, "archivo_huella_excreta_extra")) %>%
      select(id, huella_excreta_id = huella_excreta_extra_id, archivo) %>%
      inner_join(huella_extra, by = c("huella_excreta_id")) %>%
      mutate(
        tabla = "huella_excreta_extra",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl,
          "/", substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/registros_extra/", nuevo_nombre, sep = "")
      ) %>%
      group_by(path_archivos_cluster) %>%
      mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

    final_table <- ungroup(rbind(archivo_huella_trans,
      archivo_huella_extra)) %>%
      select(id, huella_excreta_id, conglomerado_muestra_id, cgl, lat, lon,
        fecha_visita, institucion, estado, municipio, monitoreo_tipo,
        vegetacion_tipo, perturbado, tabla, transecto_numero, comentario,
        nombre_comun, nombre_cientifico, es_huella, largo, ancho,
        esta_dentro_conglomerado, path_imagen_cluster, path_formato_cluster)
    }
  final_table
}

#' @rdname querySpecies
querySpecimens <- function(database, state = "all", organization = "all",
  cgl_id = "all", year_visit = 2010:2016, month_visit = 1:12, noms = "all") {

  # base table is computed with the queryCgls function
  cgl_table <- queryCgls(database, state, organization, cgl_id, year_visit,
    month_visit)

  if(nrow(cgl_table) == 0){
    # RPostgreSQL::dbDisconnect(database$con)
    print("No hay registros que cumplan los requisitos solicitados.")
    final_table <- "No hay registros que cumplan los requisitos solicitados."
  }else{
    if(noms == "all"){
      specimen_ex_aux <- tbl(database, "especimen_restos_extra") %>%
        collect() %>%
        select(nombre_comun, nombre_cientifico)
      specimens <- unique(c(specimen_ex_aux$nombre_comun,
        specimen_ex_aux$nombre_cientifico))
      specimens <- specimens[!is.na(specimens) & specimens != "NA"]
      noms <- paste(specimens, collapse = "|")
    }

    # registros de especimen/restos correspondientes a registros extra
    specimen_extra <- collect(tbl(database, "especimen_restos_extra")) %>%
      inner_join(cgl_table, by = c("conglomerado_muestra_id"))  %>%
      filter(grepl(noms, nombre_comun, ignore.case = TRUE) |
          grepl(noms, nombre_cientifico, ignore.case = TRUE)) %>%
      select(conglomerado_muestra_id, cgl, lat, lon, fecha_visita, institucion,
        estado, municipio, monitoreo_tipo, vegetacion_tipo, perturbado,
        comentario, especimen_restos_extra_id = id,
        es_especimen, nombre_comun, nombre_cientifico, numero_individuos,
        esta_dentro_conglomerado)

    archivo_especimen_extra <- collect(
      tbl(database, "archivo_especimen_restos_extra")) %>%
      select(-archivo_nombre_original) %>%
      inner_join(specimen_extra, by = c("especimen_restos_extra_id")) %>%
      mutate(
        tabla = "especimen_restos_extra",
        nuevo_nombre = gsub("(.*\\.).*\\.(.*\\.).*\\.", "\\1\\2", archivo),
        path_archivos_cluster = paste(ruta_archivos_cluster, cgl,
          "/", substr(fecha_visita, 1, 4), "_", substr(fecha_visita, 6, 7),
          sep = ""),
        path_imagen_cluster = paste(path_archivos_cluster,
          "/registros_extra/", nuevo_nombre, sep = "")
      ) %>%
      group_by(path_archivos_cluster) %>%
      mutate(
        path_formato_cluster =  list.files(path = path_archivos_cluster,
          pattern = ".pdf", ignore.case = TRUE, full.names = TRUE)[1]
      )

      final_table <- ungroup(archivo_especimen_extra) %>%
        select(id, especimen_restos_extra_id, conglomerado_muestra_id, cgl, lat,
        lon, fecha_visita, institucion, estado, municipio, monitoreo_tipo,
        vegetacion_tipo, perturbado, tabla, comentario,
        nombre_comun, nombre_cientifico, es_especimen, numero_individuos,
        esta_dentro_conglomerado, path_imagen_cluster, path_formato_cluster)
    }
  final_table
}


