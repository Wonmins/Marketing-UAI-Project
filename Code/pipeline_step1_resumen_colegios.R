#!/usr/bin/env Rscript

# =============================================================================
#  PIPELINE STEP 1: RESUMEN_COLEGIOS
# =============================================================================
#  Objetivo del script:
#    1. Cargar los datos base del DEMRE (postulaciones, matrícula, inscritos,
#       directorio y catálogo de carreras).
#    2. Homologar los códigos de carrera a las cinco categorías estratégicas
#       (Comercial, Civil, Derecho, Psicología y Periodismo) utilizando un
#       archivo de mapeo manual (`career_mapping_categoria_detalle.csv`).
#    3. Calcular indicadores por colegio (postulantes, matriculados, métricas
#       socioeconómicas, market share filtrado) y por carrera.
#    4. Enriquecer con geocodificación y generar identificadores únicos por
#       sede cuando un colegio opere en varias locaciones.
#    5. Exportar los archivos necesarios para consumo del paso 2.
#
#  Entrada esperada (carpeta `datos/`):
#    - PostulaciónySelección_AdmisiónYYYY/ArchivoD_AdmYYYY.csv
#    - PROCESO-DE-ADMISIÓN-YYYY-MATRÍCULA-17-12-YYYYT14-38-28 3/ArchivoMatr_AdmYYYY.csv
#    - Inscritos_AdmisiónYYYY 2/ArchivoB_AdmYYYY.csv
#    - directorio.csv
#    - Libro_CódigosAD MYYYY_ArchivoD.csv
#    - career_mapping_categoria_detalle.csv (mapeo manual de carreras)
#    - colegios_coordenadas_completas.csv (opcional: lat/long por sede)
#
#  Salidas principales:
#    - RESUMEN_COLEGIOS_YYYY.csv                     (base consolidada por sede)
#    - RESUMEN_COLEGIOS_CON_COORDENADAS_YYYY.csv     (incluye columnas de geocodificación)
#    - RESUMEN_UAI_DETALLADO_POR_CARRERA_YYYY.csv    (detalle por colegio-carrera)
#
#  Ejecución:
#    Rscript pipeline_step1_resumen_colegios.R 2024
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringi)
})

# -----------------------------------------------------------------------------
# Función utilitaria: carga el mapeo manual de carreras y lo cruza con el
# catálogo oficial del DEMRE para obtener vectores de códigos por categoría.
# -----------------------------------------------------------------------------
load_career_code_sets <- function(catalog,
                                  mapping_path,
                                  expected_categories = c("COMERCIAL", "ING_CIVIL", "DERECHO", "PSICOLOGIA", "PERIODISMO")) {
  required_cols <- c("CODIGO_CARRERA", "NOMBRE_CARRERA")
  if (!all(required_cols %in% names(catalog))) {
    stop("Catalog must include CODIGO_CARRERA and NOMBRE_CARRERA columns.")
  }
  
  catalog_norm <- catalog %>%
    mutate(
      NOMBRE_CARRERA_NORMALIZADO = stri_trans_general(
        toupper(NOMBRE_CARRERA),
        "Latin-ASCII"
      )
    )
  
  mapping_raw <- fread(mapping_path, encoding = "UTF-8")
  if (!all(c("CATEGORIA", "CODIGO_CARRERA") %in% names(mapping_raw))) {
    stop("Mapping file must include CATEGORIA and CODIGO_CARRERA columns.")
  }
  
  mapping_clean <- mapping_raw %>%
    mutate(
      CATEGORIA = toupper(trimws(CATEGORIA)),
      CODIGO_CARRERA = as.integer(CODIGO_CARRERA)
    ) %>%
    filter(!is.na(CODIGO_CARRERA))
  
  missing_categories <- setdiff(expected_categories, unique(mapping_clean$CATEGORIA))
  if (length(missing_categories) > 0) {
    stop("Mapping file missing categories: ", paste(missing_categories, collapse = ", "))
  }
  
  code_sets <- list()
  for (cat in expected_categories) {
    codes <- unique(mapping_clean$CODIGO_CARRERA[mapping_clean$CATEGORIA == cat])
    catalog_missing <- setdiff(codes, catalog_norm$CODIGO_CARRERA)
    if (length(catalog_missing) > 0) {
      warning(
        "Codes in mapping not found in catalog for category ",
        cat, ": ",
        paste(catalog_missing, collapse = ", ")
      )
    }
    code_sets[[cat]] <- codes
  }
  
  code_sets$CATALOG_NORMALIZED <- catalog_norm
  code_sets$MAPPING <- mapping_clean
  code_sets
}

# -----------------------------------------------------------------------------
# Parámetros de ejecución (directorios, archivos y año objetivo)
# -----------------------------------------------------------------------------
project_root <- normalizePath(".", winslash = "/")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && !is.na(suppressWarnings(as.integer(args[1])))) {
  pipeline_year <- as.integer(args[1])
} else {
  pipeline_year <- 2024L
  cat("No se proporcionó año como argumento, usando año por defecto:", pipeline_year, "\n")
}
cat("Año del pipeline:", pipeline_year, "\n")

# Función auxiliar para buscar archivos si no se encuentran en la ruta esperada
# Solo busca en ubicaciones específicas conocidas para evitar archivos incorrectos
find_file <- function(expected_path, alternative_dirs, exact_filename, data_dir) {
  if (file.exists(expected_path)) {
    return(expected_path)
  }
  
  # Si no existe, buscar solo en directorios alternativos específicos
  for (alt_dir in alternative_dirs) {
    if (alt_dir == ".") {
      # Caso especial: buscar directamente en data_dir
      alt_path <- file.path(data_dir, exact_filename)
    } else {
      alt_path <- file.path(data_dir, alt_dir, exact_filename)
    }
    if (file.exists(alt_path)) {
      cat(sprintf("  Archivo no encontrado en ruta esperada: %s\n", expected_path))
      cat(sprintf("  Usando archivo encontrado en ubicación alternativa: %s\n", alt_path))
      return(alt_path)
    }
  }
  
  # Si no se encuentra en ninguna ubicación esperada, devolver la ruta original
  # para que el error sea claro
  return(expected_path)
}

config <- list(
  year = pipeline_year,
  data_dir = file.path(project_root, "CSV"),
  output_dir = project_root
)

# Construir rutas esperadas (archivos ahora están directamente en CSV/)
expected_postulaciones <- file.path(config$data_dir, sprintf("ArchivoD_Adm%d.csv", pipeline_year))
expected_matriculados <- file.path(config$data_dir, sprintf("ArchivoMatr_Adm%d.csv", pipeline_year))
expected_inscritos <- file.path(config$data_dir, sprintf("ArchivoB_Adm%d.csv", pipeline_year))
expected_directorio <- file.path(config$data_dir, "directorio.csv")
expected_carreras <- file.path(config$data_dir, sprintf("Libro_CódigosADM%d_ArchivoD.csv", pipeline_year))

# Archivos ahora están directamente en CSV/ - usar rutas simples
config$postulaciones_file <- expected_postulaciones
config$matriculados_file <- expected_matriculados
config$inscritos_file <- expected_inscritos
config$directorio_file <- expected_directorio
config$carreras_file <- expected_carreras
config$geocodes_file <- file.path(project_root, "CSV", "colegios_coordenadas_completas.csv")
config$career_mapping_file <- file.path(project_root, "CSV", "career_mapping_categoria_detalle.csv")
config$directorio_oficial_file <- file.path(project_root, "20250926_Directorio_Oficial_EE_2025_20250430_WEB.csv")

# Verificar archivos críticos
if (!file.exists(config$postulaciones_file)) {
  stop(sprintf("Archivo de postulaciones no encontrado: %s", config$postulaciones_file))
}
if (!file.exists(config$matriculados_file)) {
  stop(sprintf("Archivo de matriculados no encontrado: %s", config$matriculados_file))
}
if (!file.exists(config$inscritos_file)) {
  stop(sprintf("Archivo de inscritos no encontrado: %s", config$inscritos_file))
}
if (!file.exists(config$directorio_file)) {
  stop(sprintf("Archivo de directorio no encontrado: %s", config$directorio_file))
}
if (!file.exists(config$carreras_file)) {
  stop(sprintf("Archivo de carreras no encontrado: %s", config$carreras_file))
}
if (!file.exists(config$career_mapping_file)) {
  stop(sprintf("Archivo de mapeo de carreras no encontrado: %s", config$career_mapping_file))
}

cat("=== PIPELINE STEP 1: RESUMEN COLEGIOS ===\n")
cat("Working directory:", project_root, "\n")
cat("Academic year:", pipeline_year, "\n\n")

# -----------------------------------------------------------------------------
# 1. Carga de insumos base del DEMRE
# -----------------------------------------------------------------------------
cat("Loading raw datasets...\n")
postulaciones_raw <- fread(config$postulaciones_file, encoding = "UTF-8")
matriculados_raw <- fread(config$matriculados_file, encoding = "UTF-8")
inscritos_raw <- fread(config$inscritos_file, encoding = "UTF-8")
directorio_raw <- fread(config$directorio_file, encoding = "UTF-8")
carreras_raw <- fread(config$carreras_file, encoding = "UTF-8")
cat("  - postulaciones:", nrow(postulaciones_raw), "rows\n")
cat("  - matriculados :", nrow(matriculados_raw), "rows\n")
cat("  - inscritos    :", nrow(inscritos_raw), "rows\n")
cat("  - directorio   :", nrow(directorio_raw), "rows\n")
cat("  - carreras     :", nrow(carreras_raw), "rows\n\n")

cat("Preparing catalogs and mappings...\n")
# 2. Combinar catálogos de carreras con el mapeo manual para identificar códigos
#    afines a cada categoría estratégica.
career_codes <- load_career_code_sets(carreras_raw, config$career_mapping_file)
carreras_norm <- career_codes$CATALOG_NORMALIZED
COD_COMERCIAL <- career_codes$COMERCIAL
COD_ING_CIVIL <- career_codes$ING_CIVIL
COD_DERECHO <- career_codes$DERECHO
COD_PSICOLOGIA <- career_codes$PSICOLOGIA
COD_PERIODISMO <- career_codes$PERIODISMO
COD_TODO <- unique(c(COD_COMERCIAL, COD_ING_CIVIL, COD_DERECHO, COD_PSICOLOGIA, COD_PERIODISMO))

cat(sprintf("  - Ingenieria Comercial: %d codigos\n", length(COD_COMERCIAL)))
cat(sprintf("  - Ingenieria Civil    : %d codigos\n", length(COD_ING_CIVIL)))
cat(sprintf("  - Derecho             : %d codigos\n", length(COD_DERECHO)))
cat(sprintf("  - Psicologia          : %d codigos\n", length(COD_PSICOLOGIA)))
cat(sprintf("  - Periodismo/Comunic.: %d codigos\n", length(COD_PERIODISMO)))
cat(sprintf("  - Total filtradas     : %d codigos\n\n", length(COD_TODO)))

COMPT1_CODES <- c(11, 12, 14, 15, 16)
COMPT2_CODES <- c(43, 44)
UAI_CODE <- 42
ESPECIAL_COMPT2 <- 41

# 3. Limpieza de insumos y renombrado de columnas clave para facilitar joins.
directorio <- directorio_raw %>%
  select(AGNO, RBD, DGV_RBD, NOM_RBD, COD_REG_RBD, NOM_REG_RBD_A) %>%
  rename(
    REGION_RBD = COD_REG_RBD,
    REGION_NOMBRE = NOM_REG_RBD_A
  )

inscritos <- inscritos_raw %>%
  select(ID_aux, RBD, ANYO_EGRESO, CODIGO_REGION, INGRESO_PERCAPITA_GRUPO_FA, SEXO, CODIGO_COMUNA)

postulaciones <- postulaciones_raw %>%
  select(-ESTADO_PREF, -TIPO_PREF) %>%
  rename(
    PREFERENCIA = ORDEN_PREF,
    CODIGO_CARRERA = COD_CARRERA_PREF
  )

matriculados <- matriculados_raw %>%
  select(-VIA, -TIPO_MATRICULA) %>%
  rename(
    CODIGO_CARRERA = CODIGO,
    PREFERENCIA = PREFERENCIA
  )

carreras <- carreras_norm %>%
  select(
    CODIGO_CARRERA,
    UNI_CODIGO,
    NOMBRE_CARRERA,
    NOMBRE_UNIVERSIDAD,
    REG_CODIGO
  ) %>%
  rename(
    CODIGO_UNIV = UNI_CODIGO,
    REGION_UNI = REG_CODIGO
  )

# -----------------------------------------------------------------------------
# 4. Construcción de tablas base (postulaciones / matrículas con atributos
#    de estudiantes, geografía y carrera).
# -----------------------------------------------------------------------------
cat("Building student-level tables...\n")
post_tabla <- postulaciones %>%
  left_join(inscritos, by = "ID_aux") %>%
  left_join(directorio, by = "RBD") %>%
  left_join(carreras, by = "CODIGO_CARRERA")

matri_tabla <- matriculados %>%
  left_join(inscritos, by = "ID_aux") %>%
  left_join(directorio, by = "RBD") %>%
  left_join(carreras, by = c("CODIGO_CARRERA", "CODIGO_UNIV"))

flag_competencia <- function(codigo_univ, region_univ, target_codes) {
  (codigo_univ %in% target_codes) | (codigo_univ == ESPECIAL_COMPT2 & region_univ == 5)
}

post_tabla <- post_tabla %>%
  mutate(
    ES_UAI = (CODIGO_UNIV == UAI_CODE),
    ES_COMP1 = CODIGO_UNIV %in% COMPT1_CODES,
    ES_COMP2 = flag_competencia(CODIGO_UNIV, REGION_UNI, COMPT2_CODES),
    ES_OTHERS = (!ES_UAI & !ES_COMP1 & !ES_COMP2),
    ES_CARRERA_FILTRADA = CODIGO_CARRERA %in% COD_TODO
  )

matri_tabla <- matri_tabla %>%
  mutate(
    ES_UAI = (CODIGO_UNIV == UAI_CODE),
    ES_COMP1 = CODIGO_UNIV %in% COMPT1_CODES,
    ES_COMP2 = flag_competencia(CODIGO_UNIV, REGION_UNI, COMPT2_CODES),
    ES_OTHERS = (!ES_UAI & !ES_COMP1 & !ES_COMP2),
    ES_CARRERA_FILTRADA = CODIGO_CARRERA %in% COD_TODO
  )

# -----------------------------------------------------------------------------
# 5. Cálculo de indicadores por colegio: totales, distribución GSE, market
#    share de competencia global y por carrera filtrada.
# -----------------------------------------------------------------------------
cat("Calculating GSE and base aggregates...\n")
PROMEDIO <- matri_tabla %>%
  group_by(RBD, NOM_RBD, REGION_RBD) %>%
  summarise(
    MATRICULADOS = n_distinct(ID_aux),
    PTJE_PROM = mean(PTJE_POND, na.rm = TRUE),
    .groups = "drop"
  )

POSTULACION <- post_tabla %>%
  group_by(RBD, NOM_RBD, REGION_RBD) %>%
  summarise(
    POSTULANTES = n_distinct(ID_aux),
    EGRESADOS_2024 = n_distinct(ID_aux[ANYO_EGRESO == pipeline_year]),
    PCNTJE_EGRESADOS_2024 = round((EGRESADOS_2024 / POSTULANTES) * 100, 2),
    POSTULANTES_UAI = n_distinct(ID_aux[ES_UAI]),
    PCNTJE_POSTULANTES_UAI = round((POSTULANTES_UAI / POSTULANTES) * 100, 2),
    TOTAL_MUJERES_POST = n_distinct(ID_aux[SEXO == 2]),
    PCNTJE_MUJERES_POST = round((TOTAL_MUJERES_POST / POSTULANTES) * 100, 2),
    POST_MUJERES_UAI = n_distinct(ID_aux[ES_UAI & SEXO == 2]),
    PCNTJE_POST_MUJERES_UAI = round((POST_MUJERES_UAI / pmax(TOTAL_MUJERES_POST, 1)) * 100, 2),
    .groups = "drop"
  )

MATRICULADOS <- matri_tabla %>%
  group_by(RBD, NOM_RBD, REGION_RBD) %>%
  summarise(
    MATRICULADOS = n_distinct(ID_aux),
    MATRICULADOS_UAI = n_distinct(ID_aux[ES_UAI]),
    PCNTJE_MATRICULADOS_UAI = round((MATRICULADOS_UAI / pmax(MATRICULADOS, 1)) * 100, 2),
    TOTAL_MUJERES_MATRI = n_distinct(ID_aux[SEXO == 2]),
    PCNTJE_MUJERES_MATRI = round((TOTAL_MUJERES_MATRI / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_MUJERES_UAI = n_distinct(ID_aux[ES_UAI & SEXO == 2]),
    PCNTJE_MATRI_MUJERES_UAI = round((MATRI_MUJERES_UAI / pmax(TOTAL_MUJERES_MATRI, 1)) * 100, 2),
    .groups = "drop"
  )

POSTyMATRI <- PROMEDIO %>%
  left_join(POSTULACION, by = c("RBD", "NOM_RBD", "REGION_RBD")) %>%
  left_join(MATRICULADOS, by = c("RBD", "NOM_RBD", "REGION_RBD", "MATRICULADOS"))

cat("Calculating socio-economic distribution...\n")
GSE <- matri_tabla %>%
  mutate(
    grupo_ingreso = case_when(
      INGRESO_PERCAPITA_GRUPO_FA %in% 1:3 ~ "Grupo 1 (1-3)",
      INGRESO_PERCAPITA_GRUPO_FA %in% 4:6 ~ "Grupo 2 (4-6)",
      INGRESO_PERCAPITA_GRUPO_FA %in% 7:9 ~ "Grupo 3 (7-9)",
      INGRESO_PERCAPITA_GRUPO_FA == 99 ~ "Grupo 4 (No responde)",
      TRUE ~ "Otro"
    )
  ) %>%
  group_by(RBD, grupo_ingreso) %>%
  summarise(cantidad = n_distinct(ID_aux), .groups = "drop") %>%
  pivot_wider(
    names_from = grupo_ingreso,
    values_from = cantidad,
    values_fill = 0
  )

POSTyMATRI <- POSTyMATRI %>%
  left_join(GSE, by = "RBD")

cat("Calculating competition metrics (applications/matriculations)...\n")
COMPETENCIAS_POST <- post_tabla %>%
  group_by(RBD, NOM_RBD, REGION_RBD) %>%
  summarise(
    POSTULANTES = n_distinct(ID_aux),
    POST_COMPETENCIA1 = n_distinct(ID_aux[ES_COMP1]),
    PCNTJE_POST_COMPETENCIA1 = round((POST_COMPETENCIA1 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_COMPETENCIA2 = n_distinct(ID_aux[ES_COMP2]),
    PCNTJE_POST_COMPETENCIA2 = round((POST_COMPETENCIA2 / pmax(POSTULANTES, 1)) * 100, 2),
    .groups = "drop"
  )

COMPETENCIAS_MATRI <- matri_tabla %>%
  group_by(RBD, NOM_RBD, REGION_RBD) %>%
  summarise(
    MATRICULADOS = n_distinct(ID_aux),
    MATRI_COMPETENCIA1 = n_distinct(ID_aux[ES_COMP1]),
    PCNTJE_MATRI_COMPETENCIA1 = round((MATRI_COMPETENCIA1 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_COMPETENCIA2 = n_distinct(ID_aux[ES_COMP2]),
    PCNTJE_MATRI_COMPETENCIA2 = round((MATRI_COMPETENCIA2 / pmax(MATRICULADOS, 1)) * 100, 2),
    .groups = "drop"
  )

POSTyMATRI <- POSTyMATRI %>%
  left_join(COMPETENCIAS_POST, by = c("RBD", "NOM_RBD", "REGION_RBD", "POSTULANTES")) %>%
  left_join(COMPETENCIAS_MATRI, by = c("RBD", "NOM_RBD", "REGION_RBD", "MATRICULADOS"))

cat("Calculating career-specific indicators...\n")
ESPECIFICACION_CARRERAS <- post_tabla %>%
  group_by(RBD, NOM_RBD, REGION_RBD) %>%
  summarise(
    POSTULANTES = n_distinct(ID_aux),
    POST_UAI_FILTRADO = n_distinct(ID_aux[ES_CARRERA_FILTRADA & ES_UAI]),
    PCNTJE_POST_UAI_FILTRADO = round((POST_UAI_FILTRADO / pmax(POSTULANTES, 1)) * 100, 2),
    POST_COMERCIAL = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_COMERCIAL]),
    PCNTJE_COMERCIAL_POST = round((POST_COMERCIAL / pmax(POSTULANTES, 1)) * 100, 2),
    POST_COMERCIAL_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_COMERCIAL & ES_UAI]),
    PCNTJE_COMERCIAL_UAI_POST = round((POST_COMERCIAL_UAI / pmax(POSTULANTES, 1)) * 100, 2),
    POST_COMERCIAL_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_COMERCIAL & ES_COMP1]),
    PCNTJE_COMERCIAL_COMPT1_POST = round((POST_COMERCIAL_COMPT1 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_COMERCIAL_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_COMERCIAL & ES_COMP2]),
    PCNTJE_COMERCIAL_COMPT2_POST = round((POST_COMERCIAL_COMPT2 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_ING_CIVIL = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_ING_CIVIL]),
    PCNTJE_ING_CIVIL_POST = round((POST_ING_CIVIL / pmax(POSTULANTES, 1)) * 100, 2),
    POST_ING_CIVIL_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_ING_CIVIL & ES_UAI]),
    PCNTJE_ING_CIVIL_UAI_POST = round((POST_ING_CIVIL_UAI / pmax(POSTULANTES, 1)) * 100, 2),
    POST_ING_CIVIL_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_ING_CIVIL & ES_COMP1]),
    PCNTJE_ING_CIVIL_COMPT1_POST = round((POST_ING_CIVIL_COMPT1 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_ING_CIVIL_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_ING_CIVIL & ES_COMP2]),
    PCNTJE_ING_CIVIL_COMPT2_POST = round((POST_ING_CIVIL_COMPT2 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_DERECHO = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_DERECHO]),
    PCNTJE_DERECHO_POST = round((POST_DERECHO / pmax(POSTULANTES, 1)) * 100, 2),
    POST_DERECHO_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_DERECHO & ES_UAI]),
    PCNTJE_DERECHO_UAI_POST = round((POST_DERECHO_UAI / pmax(POSTULANTES, 1)) * 100, 2),
    POST_DERECHO_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_DERECHO & ES_COMP1]),
    PCNTJE_DERECHO_COMPT1_POST = round((POST_DERECHO_COMPT1 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_DERECHO_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_DERECHO & ES_COMP2]),
    PCNTJE_DERECHO_COMPT2_POST = round((POST_DERECHO_COMPT2 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_PSICOLOGIA = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PSICOLOGIA]),
    PCNTJE_PSICOLOGIA_POST = round((POST_PSICOLOGIA / pmax(POSTULANTES, 1)) * 100, 2),
    POST_PSICOLOGIA_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PSICOLOGIA & ES_UAI]),
    PCNTJE_PSICOLOGIA_UAI_POST = round((POST_PSICOLOGIA_UAI / pmax(POSTULANTES, 1)) * 100, 2),
    POST_PSICOLOGIA_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PSICOLOGIA & ES_COMP1]),
    PCNTJE_PSICOLOGIA_COMPT1_POST = round((POST_PSICOLOGIA_COMPT1 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_PSICOLOGIA_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PSICOLOGIA & ES_COMP2]),
    PCNTJE_PSICOLOGIA_COMPT2_POST = round((POST_PSICOLOGIA_COMPT2 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_PERIODISMO = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PERIODISMO]),
    PCNTJE_PERIODISMO_POST = round((POST_PERIODISMO / pmax(POSTULANTES, 1)) * 100, 2),
    POST_PERIODISMO_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PERIODISMO & ES_UAI]),
    PCNTJE_PERIODISMO_UAI_POST = round((POST_PERIODISMO_UAI / pmax(POSTULANTES, 1)) * 100, 2),
    POST_PERIODISMO_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PERIODISMO & ES_COMP1]),
    PCNTJE_PERIODISMO_COMPT1_POST = round((POST_PERIODISMO_COMPT1 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_PERIODISMO_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PERIODISMO & ES_COMP2]),
    PCNTJE_PERIODISMO_COMPT2_POST = round((POST_PERIODISMO_COMPT2 / pmax(POSTULANTES, 1)) * 100, 2),
    POST_COMPT1_FILTRADO = n_distinct(ID_aux[ES_CARRERA_FILTRADA & ES_COMP1]),
    PCNTJE_COMPT1_POST_FILTRADO = round((POST_COMPT1_FILTRADO / pmax(POSTULANTES, 1)) * 100, 2),
    POST_COMPT2_FILTRADO = n_distinct(ID_aux[ES_CARRERA_FILTRADA & ES_COMP2]),
    PCNTJE_COMPT2_POST_FILTRADO = round((POST_COMPT2_FILTRADO / pmax(POSTULANTES, 1)) * 100, 2),
    .groups = "drop"
  )

ESPECIFICACION_CARRERAS2 <- matri_tabla %>%
  group_by(RBD, NOM_RBD, REGION_RBD) %>%
  summarise(
    MATRICULADOS = n_distinct(ID_aux),
    MATRI_UAI_FILTRADO = n_distinct(ID_aux[ES_CARRERA_FILTRADA & ES_UAI]),
    PCNTJE_MATRI_UAI_FILTRADO = round((MATRI_UAI_FILTRADO / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_COMERCIAL = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_COMERCIAL]),
    PCNTJE_COMERCIAL_MATRI = round((MATRI_COMERCIAL / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_COMERCIAL_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_COMERCIAL & ES_UAI]),
    PCNTJE_COMERCIAL_UAI_MATRI = round((MATRI_COMERCIAL_UAI / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_COMERCIAL_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_COMERCIAL & ES_COMP1]),
    PCNTJE_COMERCIAL_COMPT1_MATRI = round((MATRI_COMERCIAL_COMPT1 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_COMERCIAL_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_COMERCIAL & ES_COMP2]),
    PCNTJE_COMERCIAL_COMPT2_MATRI = round((MATRI_COMERCIAL_COMPT2 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_ING_CIVIL = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_ING_CIVIL]),
    PCNTJE_ING_CIVIL_MATRI = round((MATRI_ING_CIVIL / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_ING_CIVIL_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_ING_CIVIL & ES_UAI]),
    PCNTJE_ING_CIVIL_UAI_MATRI = round((MATRI_ING_CIVIL_UAI / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_ING_CIVIL_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_ING_CIVIL & ES_COMP1]),
    PCNTJE_ING_CIVIL_COMPT1_MATRI = round((MATRI_ING_CIVIL_COMPT1 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_ING_CIVIL_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_ING_CIVIL & ES_COMP2]),
    PCNTJE_ING_CIVIL_COMPT2_MATRI = round((MATRI_ING_CIVIL_COMPT2 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_DERECHO = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_DERECHO]),
    PCNTJE_DERECHO_MATRI = round((MATRI_DERECHO / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_DERECHO_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_DERECHO & ES_UAI]),
    PCNTJE_DERECHO_UAI_MATRI = round((MATRI_DERECHO_UAI / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_DERECHO_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_DERECHO & ES_COMP1]),
    PCNTJE_DERECHO_COMPT1_MATRI = round((MATRI_DERECHO_COMPT1 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_DERECHO_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_DERECHO & ES_COMP2]),
    PCNTJE_DERECHO_COMPT2_MATRI = round((MATRI_DERECHO_COMPT2 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_PSICOLOGIA = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PSICOLOGIA]),
    PCNTJE_PSICOLOGIA_MATRI = round((MATRI_PSICOLOGIA / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_PSICOLOGIA_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PSICOLOGIA & ES_UAI]),
    PCNTJE_PSICOLOGIA_UAI_MATRI = round((MATRI_PSICOLOGIA_UAI / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_PSICOLOGIA_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PSICOLOGIA & ES_COMP1]),
    PCNTJE_PSICOLOGIA_COMPT1_MATRI = round((MATRI_PSICOLOGIA_COMPT1 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_PSICOLOGIA_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PSICOLOGIA & ES_COMP2]),
    PCNTJE_PSICOLOGIA_COMPT2_MATRI = round((MATRI_PSICOLOGIA_COMPT2 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_PERIODISMO = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PERIODISMO]),
    PCNTJE_PERIODISMO_MATRI = round((MATRI_PERIODISMO / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_PERIODISMO_UAI = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PERIODISMO & ES_UAI]),
    PCNTJE_PERIODISMO_UAI_MATRI = round((MATRI_PERIODISMO_UAI / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_PERIODISMO_COMPT1 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PERIODISMO & ES_COMP1]),
    PCNTJE_PERIODISMO_COMPT1_MATRI = round((MATRI_PERIODISMO_COMPT1 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_PERIODISMO_COMPT2 = n_distinct(ID_aux[CODIGO_CARRERA %in% COD_PERIODISMO & ES_COMP2]),
    PCNTJE_PERIODISMO_COMPT2_MATRI = round((MATRI_PERIODISMO_COMPT2 / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_COMPT1_FILTRADO = n_distinct(ID_aux[ES_CARRERA_FILTRADA & ES_COMP1]),
    PCNTJE_COMPT1_MATRI_FILTRADO = round((MATRI_COMPT1_FILTRADO / pmax(MATRICULADOS, 1)) * 100, 2),
    MATRI_COMPT2_FILTRADO = n_distinct(ID_aux[ES_CARRERA_FILTRADA & ES_COMP2]),
    PCNTJE_COMPT2_MATRI_FILTRADO = round((MATRI_COMPT2_FILTRADO / pmax(MATRICULADOS, 1)) * 100, 2),
    .groups = "drop"
  )

POSTyMATRI <- POSTyMATRI %>%
  left_join(ESPECIFICACION_CARRERAS, by = c("RBD", "NOM_RBD", "REGION_RBD", "POSTULANTES")) %>%
  left_join(ESPECIFICACION_CARRERAS2, by = c("RBD", "NOM_RBD", "REGION_RBD", "MATRICULADOS"))

# -----------------------------------------------------------------------------
# 6. Filtrado mínimo de volumen (POSTULANTES >= 10) para estabilizar porcentajes
#    y, opcionalmente, enriquecimiento con geocodificación.
# -----------------------------------------------------------------------------
cat("Applying minimum threshold (POSTULANTES >= 10)...\n")
POSTyMATRI <- POSTyMATRI %>%
  filter(POSTULANTES >= 10) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 6.1. Cruce con Directorio Oficial para obtener coordenadas (LATITUD/LONGITUD)
# -----------------------------------------------------------------------------
cat("Merging coordinates from Directorio Oficial...\n")
if (file.exists(config$directorio_oficial_file)) {
  directorio_oficial_raw <- fread(config$directorio_oficial_file, encoding = "UTF-8", sep = ";")
  
  # Verificar que tenga las columnas necesarias
  required_cols <- c("RBD", "LATITUD", "LONGITUD")
  if (all(required_cols %in% names(directorio_oficial_raw))) {
    # Extraer y limpiar coordenadas del directorio oficial
    directorio_coords <- directorio_oficial_raw %>%
      select(RBD, LATITUD, LONGITUD, NOM_COM_RBD, NOM_REG_RBD_A) %>%
      mutate(
        RBD = as.integer(RBD),
        # Convertir coordenadas de formato con comas a formato numérico
        # Reemplazar comas por puntos y convertir a numérico
        LATITUD = as.numeric(gsub(",", ".", LATITUD)),
        LONGITUD = as.numeric(gsub(",", ".", LONGITUD)),
        # Agregar prefijo para identificar fuente
        LATITUD_DIRECTORIO = LATITUD,
        LONGITUD_DIRECTORIO = LONGITUD,
        COMUNA_DIRECTORIO = NOM_COM_RBD,
        REGION_DIRECTORIO = NOM_REG_RBD_A
      ) %>%
      select(RBD, LATITUD_DIRECTORIO, LONGITUD_DIRECTORIO, COMUNA_DIRECTORIO, REGION_DIRECTORIO) %>%
      filter(!is.na(RBD))
    
    # Contar cuántos RBDs tienen coordenadas válidas en el directorio
    directorio_con_coords <- directorio_coords %>%
      filter(!is.na(LATITUD_DIRECTORIO) & !is.na(LONGITUD_DIRECTORIO) &
             LATITUD_DIRECTORIO != 0 & LONGITUD_DIRECTORIO != 0)
    cat(sprintf("  - RBDs con coordenadas válidas en Directorio Oficial: %d\n", nrow(directorio_con_coords)))
    
    # Hacer el merge con POSTyMATRI
    POSTyMATRI <- POSTyMATRI %>%
      mutate(RBD = as.integer(RBD)) %>%
      left_join(directorio_coords, by = "RBD")
    
    # Actualizar LATITUD y LONGITUD con valores del directorio
    # Como POSTyMATRI no tiene LATITUD/LONGITUD aún, simplemente asignamos las del directorio
    POSTyMATRI <- POSTyMATRI %>%
      mutate(
        LATITUD = LATITUD_DIRECTORIO,
        LONGITUD = LONGITUD_DIRECTORIO,
        COMUNA = coalesce(COMUNA_DIRECTORIO, as.character(REGION_RBD)),
        REGION_GEO_NOMBRE = coalesce(REGION_DIRECTORIO, as.character(REGION_RBD))
      )
    
    # Contar cuántos colegios quedaron sin mapear después del merge
    colegios_sin_coords <- POSTyMATRI %>%
      filter(is.na(LATITUD) | is.na(LONGITUD) | LATITUD == 0 | LONGITUD == 0) %>%
      summarise(total = n(), rbd_unicos = n_distinct(RBD))
    
    cat(sprintf("  - Colegios sin coordenadas después del merge: %d (RBDs únicos: %d)\n", 
                colegios_sin_coords$total, colegios_sin_coords$rbd_unicos))
    cat(sprintf("  - Colegios con coordenadas: %d (RBDs únicos: %d)\n",
                sum(!is.na(POSTyMATRI$LATITUD) & !is.na(POSTyMATRI$LONGITUD) & 
                    POSTyMATRI$LATITUD != 0 & POSTyMATRI$LONGITUD != 0),
                n_distinct(POSTyMATRI$RBD[!is.na(POSTyMATRI$LATITUD) & !is.na(POSTyMATRI$LONGITUD) & 
                                         POSTyMATRI$LATITUD != 0 & POSTyMATRI$LONGITUD != 0])))
  } else {
    warning("Directorio Oficial file found but lacks required columns (RBD, LATITUD, LONGITUD); skipping merge.")
  }
} else {
  cat("Directorio Oficial file not found, skipping coordinates merge.\n")
}

# -----------------------------------------------------------------------------
# 6.2. Complementar con archivo de geocodificación adicional si existe
# -----------------------------------------------------------------------------
if (file.exists(config$geocodes_file)) {
  cat("Merging additional geocoding information...\n")
  geocodes_raw <- fread(config$geocodes_file, encoding = "UTF-8")
  if (!"RBD" %in% names(geocodes_raw)) {
    warning("Geocode file found but lacks required RBD column; skipping geocoding merge.")
  } else {
    geocodes_raw <- geocodes_raw %>%
      mutate(RBD = as.integer(RBD))
    geocode_names <- toupper(names(geocodes_raw))
    setnames(geocodes_raw, names(geocodes_raw), geocode_names)
    geocodes <- geocodes_raw %>%
      select(any_of(c(
        "RBD", "NOMBRE_COLEGIO", "COMUNA", "REGION_NUMERO", "REGION_NOMBRE",
        "LATITUD", "LONGITUD", "DIRECCION", "NUMERO", "PROVINCIA",
        "REFERENCIA", "ESTADO_GEOCODIFICACION"
      ))) %>%
      rename(
        GEOCODE_NOMBRE = NOMBRE_COLEGIO,
        REGION_GEO_NUM = REGION_NUMERO,
        REGION_GEO_NOMBRE_EXTRA = REGION_NOMBRE,
        LATITUD_EXTRA = LATITUD,
        LONGITUD_EXTRA = LONGITUD,
        COMUNA_EXTRA = COMUNA
      )
    
    # Solo actualizar coordenadas si no existen ya del directorio oficial
    POSTyMATRI <- POSTyMATRI %>%
      left_join(geocodes, by = "RBD") %>%
      mutate(
        LATITUD = coalesce(LATITUD, as.numeric(LATITUD_EXTRA)),
        LONGITUD = coalesce(LONGITUD, as.numeric(LONGITUD_EXTRA)),
        COMUNA = coalesce(COMUNA, COMUNA_EXTRA, as.character(REGION_RBD)),
        REGION_GEO_NOMBRE = coalesce(REGION_GEO_NOMBRE, REGION_GEO_NOMBRE_EXTRA, as.character(REGION_RBD)),
        REGION_GEO_NUM = coalesce(REGION_GEO_NUM, as.character(REGION_GEO_NUM), as.character(REGION_RBD))
      )
  }
} else {
  cat("Additional geocode file not found, skipping.\n")
}

# Asegurar que las coordenadas estén en formato numérico y limpiar valores
POSTyMATRI <- POSTyMATRI %>%
  mutate(
    REGION_RBD = as.character(REGION_RBD),
    LATITUD = as.numeric(LATITUD),
    LONGITUD = as.numeric(LONGITUD),
    # Marcar coordenadas inválidas (0 o NA) como NA
    LATITUD = if_else(is.na(LATITUD) | LATITUD == 0, NA_real_, LATITUD),
    LONGITUD = if_else(is.na(LONGITUD) | LONGITUD == 0, NA_real_, LONGITUD),
    COMUNA = coalesce(as.character(COMUNA), REGION_RBD),
    REGION_GEO_NOMBRE = coalesce(REGION_GEO_NOMBRE, REGION_RBD),
    REGION_GEO_NUM = coalesce(as.character(REGION_GEO_NUM), REGION_RBD)
  )

# -----------------------------------------------------------------------------
# 7. Generar identificadores únicos por sede (cuando un mismo nombre de colegio
#    posee más de un RBD) para permitir visualizaciones diferenciadas.
# -----------------------------------------------------------------------------
POSTyMATRI <- POSTyMATRI %>%
  arrange(NOM_RBD, RBD) %>%
  group_by(NOM_RBD) %>%
  mutate(
    ES_SEDE_MULTIPLE = n() > 1,
    SEDE_INDEX = row_number(),
    NOM_SEDE = if_else(
      ES_SEDE_MULTIPLE,
      paste(NOM_RBD, coalesce(COMUNA, REGION_GEO_NOMBRE, as.character(RBD)), paste0("Sede", SEDE_INDEX), sep = " - "),
      NOM_RBD
    ),
    SEDE_ID = if_else(ES_SEDE_MULTIPLE, paste0(RBD, "_", SEDE_INDEX), as.character(RBD))
  ) %>%
  ungroup() %>%
  select(-SEDE_INDEX)

# -----------------------------------------------------------------------------
# 8. Exportar resultados principales para consumo posterior (incluido Paso 2).
# -----------------------------------------------------------------------------
POSTyMATRI <- POSTyMATRI %>%
  mutate(RBD = as.integer(RBD)) %>%
  filter(!is.na(RBD))

resumen_output_year <- file.path(config$output_dir, sprintf("RESUMEN_COLEGIOS_%d.csv", pipeline_year))
resumen_output_latest <- file.path(config$output_dir, "RESUMEN_COLEGIOS.csv")
fwrite(POSTyMATRI, resumen_output_year, sep = ";")
fwrite(POSTyMATRI, resumen_output_latest, sep = ";")
cat("Saved resumen:", resumen_output_year, "\n")

# -----------------------------------------------------------------------------
# 9. Resumen detallado por colegio y carrera (postulaciones/matrícula UAI,
#    competidores y otros) útil para análisis específicos.
# -----------------------------------------------------------------------------
cat("Building carrera-level summary for UAI and competitors...\n")
resumen_carreras <- list()
catalogo_filtrado <- carreras_norm %>%
  filter(CODIGO_CARRERA %in% COD_TODO) %>%
  select(CODIGO_CARRERA, NOMBRE_CARRERA, UNI_CODIGO, NOMBRE_UNIVERSIDAD)

for (i in seq_len(nrow(catalogo_filtrado))) {
  cod_carrera <- catalogo_filtrado$CODIGO_CARRERA[i]
  nom_carrera <- catalogo_filtrado$NOMBRE_CARRERA[i]
  uni_codigo <- catalogo_filtrado$UNI_CODIGO[i]
  uni_nombre <- catalogo_filtrado$NOMBRE_UNIVERSIDAD[i]
  
  post_carrera <- post_tabla %>%
    filter(CODIGO_CARRERA == cod_carrera) %>%
    group_by(RBD, NOM_RBD, REGION_RBD) %>%
    summarise(
      CODIGO_CARRERA = first(cod_carrera),
      NOMBRE_CARRERA = first(nom_carrera),
      CODIGO_UNIV = first(uni_codigo),
      NOMBRE_UNIV = first(uni_nombre),
      POST_UAI = n_distinct(ID_aux[ES_UAI]),
      POST_COMP1 = n_distinct(ID_aux[ES_COMP1]),
      POST_COMP2 = n_distinct(ID_aux[ES_COMP2]),
      POST_OTHERS = n_distinct(ID_aux[ES_OTHERS]),
      POST_TOTAL = n_distinct(ID_aux),
      .groups = "drop"
    )
  
  matri_carrera <- matri_tabla %>%
    filter(CODIGO_CARRERA == cod_carrera) %>%
    group_by(RBD, NOM_RBD, REGION_RBD) %>%
    summarise(
      MATRI_UAI = n_distinct(ID_aux[ES_UAI]),
      MATRI_COMP1 = n_distinct(ID_aux[ES_COMP1]),
      MATRI_COMP2 = n_distinct(ID_aux[ES_COMP2]),
      MATRI_OTHERS = n_distinct(ID_aux[ES_OTHERS]),
      MATRI_TOTAL = n_distinct(ID_aux),
      .groups = "drop"
    )
  
  resumen_carreras[[i]] <- post_carrera %>%
    full_join(matri_carrera, by = c("RBD", "NOM_RBD", "REGION_RBD"))
}

RESUMEN_FINAL_CARRERAS <- bind_rows(resumen_carreras) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  arrange(RBD, CODIGO_CARRERA)

resumen_carreras_output_year <- file.path(config$output_dir, sprintf("RESUMEN_UAI_DETALLADO_POR_CARRERA_%d.csv", pipeline_year))
resumen_carreras_output_latest <- file.path(config$output_dir, "RESUMEN_UAI_DETALLADO_POR_CARRERA.csv")
fwrite(RESUMEN_FINAL_CARRERAS, resumen_carreras_output_year, sep = ";")
fwrite(RESUMEN_FINAL_CARRERAS, resumen_carreras_output_latest, sep = ";")
cat("Saved carrera detail:", resumen_carreras_output_year, "\n")

if (!is.null(config$geocodes_file) && file.exists(config$geocodes_file)) {
  resumen_geo_output_year <- file.path(config$output_dir, sprintf("RESUMEN_COLEGIOS_CON_COORDENADAS_%d.csv", pipeline_year))
  resumen_geo_output_latest <- file.path(config$output_dir, "RESUMEN_COLEGIOS_CON_COORDENADAS.csv")
  fwrite(POSTyMATRI, resumen_geo_output_year, sep = ";")
  fwrite(POSTyMATRI, resumen_geo_output_latest, sep = ";")
  cat("Saved resumen with geocoding:", resumen_geo_output_year, "\n")
}

# -----------------------------------------------------------------------------
# Reporte final de geocodificación
# -----------------------------------------------------------------------------
colegios_con_coords <- POSTyMATRI %>%
  filter(!is.na(LATITUD) & !is.na(LONGITUD)) %>%
  summarise(total = n(), rbd_unicos = n_distinct(RBD))

colegios_sin_coords_final <- POSTyMATRI %>%
  filter(is.na(LATITUD) | is.na(LONGITUD)) %>%
  summarise(total = n(), rbd_unicos = n_distinct(RBD))

cat("\n=== STEP 1 COMPLETE ===\n")
cat("Records exported:", nrow(POSTyMATRI), "\n")
cat("Unique schools:", length(unique(POSTyMATRI$RBD)), "\n")
cat("\n=== GEOCODIFICACIÓN SUMMARY ===\n")
cat(sprintf("Colegios CON coordenadas: %d registros (%d RBDs únicos)\n", 
            colegios_con_coords$total, colegios_con_coords$rbd_unicos))
cat(sprintf("Colegios SIN coordenadas: %d registros (%d RBDs únicos)\n", 
            colegios_sin_coords_final$total, colegios_sin_coords_final$rbd_unicos))
cat(sprintf("Porcentaje mapeado: %.2f%%\n", 
            (colegios_con_coords$rbd_unicos / length(unique(POSTyMATRI$RBD))) * 100))

