library(dplyr)
library(stringi)

#' Normalize career catalog names and construct mapping to strategic buckets.
#'
#' @param catalog data.frame with at least CODIGO_CARRERA and NOMBRE_CARRERA.
#' @param pattern_list optional named list of character vectors with regex patterns
#'   (already ASCII) per bucket.
#' @return list with normalized catalog and named vectors of CODIGO_CARRERA per bucket.
build_career_code_sets <- function(catalog,
                                   pattern_list = NULL) {
  if (!all(c("CODIGO_CARRERA", "NOMBRE_CARRERA") %in% names(catalog))) {
    stop("Catalog must include CODIGO_CARRERA and NOMBRE_CARRERA columns.")
  }
  
  catalog_norm <- catalog %>%
    mutate(
      NOMBRE_CARRERA_NORMALIZADO = stri_trans_general(
        toupper(NOMBRE_CARRERA),
        "Latin-ASCII"
      )
    )
  
  default_patterns <- list(
    COMERCIAL = c("INGENIERIA\\s+COMERCIAL", "NEGOCIOS", "ADMINISTRACION"),
    ING_CIVIL = c("INGENIERIA\\s+CIVIL", "PLAN\\s+COMUN\\s+CIVIL", "CIVIL\\s+INDUSTRIAL"),
    DERECHO = c("DERECHO", "JURIDIC"),
    PSICOLOGIA = c("PSICOLOGIA"),
    PERIODISMO = c("PERIODISMO", "COMUNICACION")
  )
  
  patterns <- if (is.null(pattern_list)) default_patterns else pattern_list
  
  get_codes <- function(patterns) {
    regex <- paste(patterns, collapse = "|")
    unique(catalog_norm$CODIGO_CARRERA[
      grepl(regex, catalog_norm$NOMBRE_CARRERA_NORMALIZADO, ignore.case = FALSE, perl = TRUE)
    ])
  }
  
  code_sets <- lapply(patterns, get_codes)
  code_sets$CATALOG_NORMALIZED <- catalog_norm
  code_sets
}

