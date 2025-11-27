#!/usr/bin/env Rscript

# =============================================================================
#  PIPELINE STEP 2: CLUSTERING & SEGMENTATION
# =============================================================================
#  Responsabilidades del script:
#    - Leer el resumen enriquecido (Paso 1) y preparar columnas numéricas.
#    - Ejecutar k-means para clústeres de preferencias de carrera.
#    - Ejecutar k-means para clústeres de competencia (post/matri filtrada y sin filtrar).
#    - Calcular market share, lift y asignar el segmento competitivo final.
#    - Exportar datasets y gráficos para dashboards o mapas.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(factoextra)
  library(cluster)
})

project_root <- normalizePath(".", winslash = "/")
source(file.path(project_root, "Code", "career_mapping_utils.r"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && !is.na(suppressWarnings(as.integer(args[1])))) {
  pipeline_year <- as.integer(args[1])
} else {
  pipeline_year <- 2024L
  cat("No se proporcionó año como argumento, usando año por defecto:", pipeline_year, "\n")
}
cat("Año del pipeline:", pipeline_year, "\n")

config <- list(
  year = pipeline_year,
  input_resumen_file = file.path(project_root, "CSV", sprintf("RESUMEN_COLEGIOS_%d.csv", pipeline_year)),
  fallback_resumen_file = file.path(project_root, "CSV", "RESUMEN_COLEGIOS.csv"),
  output_dir = project_root,
  export_plots = TRUE
)

input_path <- if (file.exists(config$input_resumen_file)) {
  config$input_resumen_file
} else {
  config$fallback_resumen_file
}

stopifnot(file.exists(input_path))

cat("=== PIPELINE STEP 2: CLUSTERING & SEGMENTATION ===\n")
cat("Working directory:", project_root, "\n")
cat("Academic year:", pipeline_year, "\n")
cat("Input resumen:", input_path, "\n\n")

# 1) Normalizar dataset de entrada: asegurar tipos numéricos y nombres de sede.
schools <- fread(input_path, encoding = "UTF-8") %>%
  filter(!is.na(RBD)) %>%
  mutate(
    RBD = as.integer(RBD),
    LATITUD = as.numeric(LATITUD),
    LONGITUD = as.numeric(LONGITUD),
    NOM_SEDE = if_else(is.na(NOM_SEDE), NOM_RBD, NOM_SEDE),
    SEDE_ID = if_else(is.na(SEDE_ID), as.character(RBD), SEDE_ID),
    ES_SEDE_MULTIPLE = if_else(is.na(ES_SEDE_MULTIPLE), FALSE, ES_SEDE_MULTIPLE)
  )
# Convertir a data.frame para evitar problemas con data.table
if (inherits(schools, "data.table")) {
  schools <- as.data.frame(schools)
}
cat("Schools loaded:", nrow(schools), "\n")

# Función auxiliar para convertir columnas a numéricas sin fallar si faltan.
numericize <- function(df, columns) {
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  df
}

career_cols_matri <- c(
  "PCNTJE_COMERCIAL_MATRI", "PCNTJE_ING_CIVIL_MATRI", "PCNTJE_DERECHO_MATRI",
  "PCNTJE_PSICOLOGIA_MATRI", "PCNTJE_PERIODISMO_MATRI"
)

# Carreras estratégicas a monitorear (se reutiliza en cálculos y gráficos)
career_groups <- c("COMERCIAL", "ING_CIVIL", "DERECHO", "PSICOLOGIA", "PERIODISMO")

# 2) Preparar matriz (porcentaje de matriculados por carrera) para clustering de preferencias.
#    EXCLUIR BAJO_VOLUMEN de la clusterización
schools <- numericize(schools, career_cols_matri)

# Asegurar que las columnas necesarias existan y sean numéricas
col_numeric_base <- c(
  "MATRICULADOS", "MATRICULADOS_UAI", "MATRI_COMPETENCIA1", "MATRI_COMPETENCIA2",
  "POSTULANTES", "POSTULANTES_UAI", "POST_COMPETENCIA1", "POST_COMPETENCIA2"
)
schools <- numericize(schools, col_numeric_base)

# Calcular BAJO_VOLUMEN antes de filtrar
schools <- schools %>%
  mutate(
    # Calcular BAJO_VOLUMEN temprano para filtrar antes de clusterizar
    MATRI_OTHERS_TEMP = pmax(0, MATRICULADOS - MATRICULADOS_UAI - MATRI_COMPETENCIA1 - MATRI_COMPETENCIA2, na.rm = TRUE),
    POST_OTHERS_TEMP = pmax(0, POSTULANTES - POSTULANTES_UAI - POST_COMPETENCIA1 - POST_COMPETENCIA2, na.rm = TRUE),
    TOTAL_MARKET_MATRI_TEMP = MATRICULADOS_UAI + MATRI_COMPETENCIA1 + MATRI_COMPETENCIA2 + MATRI_OTHERS_TEMP,
    TOTAL_MARKET_POST_TEMP = POSTULANTES_UAI + POST_COMPETENCIA1 + POST_COMPETENCIA2 + POST_OTHERS_TEMP,
    ES_BAJO_VOLUMEN = (!is.na(TOTAL_MARKET_MATRI_TEMP) & TOTAL_MARKET_MATRI_TEMP < 10) &
      (!is.na(TOTAL_MARKET_POST_TEMP) & TOTAL_MARKET_POST_TEMP < 15)
  )

# Filtrar BAJO_VOLUMEN antes de clusterizar
# IMPORTANTE: Los colegios BAJO_VOLUMEN no participan en la clusterización
# ESTRATEGIA: Normalizar las 5 carreras para que sumen 100% entre ellas (comparación relativa)
# Esto permite encontrar patrones de preferencia entre las carreras estratégicas

career_cols_all <- career_cols_matri

# Preparar datos para clusterización
career_data <- schools %>%
  filter(!ES_BAJO_VOLUMEN) %>%
  mutate(
    # Calcular suma de porcentajes de las 5 carreras estratégicas
    SUMA_PCT_CAREERAS_ESTRATEGICAS = coalesce(PCNTJE_COMERCIAL_MATRI, 0) +
                                     coalesce(PCNTJE_ING_CIVIL_MATRI, 0) +
                                     coalesce(PCNTJE_DERECHO_MATRI, 0) +
                                     coalesce(PCNTJE_PSICOLOGIA_MATRI, 0) +
                                     coalesce(PCNTJE_PERIODISMO_MATRI, 0)
  ) %>%
  # Normalizar las 5 carreras para que sumen 100% entre ellas (comparación relativa)
  # Solo normalizar si hay al menos una carrera estratégica (SUMA > 0)
  mutate(
    PCNTJE_COMERCIAL_MATRI_NORM = if_else(SUMA_PCT_CAREERAS_ESTRATEGICAS > 0,
                                          (coalesce(PCNTJE_COMERCIAL_MATRI, 0) / SUMA_PCT_CAREERAS_ESTRATEGICAS) * 100,
                                          0),
    PCNTJE_ING_CIVIL_MATRI_NORM = if_else(SUMA_PCT_CAREERAS_ESTRATEGICAS > 0,
                                          (coalesce(PCNTJE_ING_CIVIL_MATRI, 0) / SUMA_PCT_CAREERAS_ESTRATEGICAS) * 100,
                                          0),
    PCNTJE_DERECHO_MATRI_NORM = if_else(SUMA_PCT_CAREERAS_ESTRATEGICAS > 0,
                                        (coalesce(PCNTJE_DERECHO_MATRI, 0) / SUMA_PCT_CAREERAS_ESTRATEGICAS) * 100,
                                        0),
    PCNTJE_PSICOLOGIA_MATRI_NORM = if_else(SUMA_PCT_CAREERAS_ESTRATEGICAS > 0,
                                           (coalesce(PCNTJE_PSICOLOGIA_MATRI, 0) / SUMA_PCT_CAREERAS_ESTRATEGICAS) * 100,
                                           0),
    PCNTJE_PERIODISMO_MATRI_NORM = if_else(SUMA_PCT_CAREERAS_ESTRATEGICAS > 0,
                                           (coalesce(PCNTJE_PERIODISMO_MATRI, 0) / SUMA_PCT_CAREERAS_ESTRATEGICAS) * 100,
                                           0)
  ) %>%
  # Excluir colegios sin ninguna carrera estratégica (todos los valores normalizados serían 0)
  filter(SUMA_PCT_CAREERAS_ESTRATEGICAS > 0) %>%
  select(RBD, NOM_RBD, all_of(career_cols_all), 
         paste0(career_cols_all, "_NORM"), 
         SUMA_PCT_CAREERAS_ESTRATEGICAS, PTJE_PROM) %>%
  drop_na(any_of(career_cols_all))

# Usar valores normalizados para clusterización (suman 100% entre las 5 carreras)
career_cols_norm <- paste0(career_cols_all, "_NORM")
career_matrix <- career_data %>%
  select(all_of(career_cols_norm))

# =============================================================================
# ANÁLISIS DE SILUETA PARA DETERMINAR K ÓPTIMO - CARRERAS
# =============================================================================
cat("\n=== ANÁLISIS DE SILUETA PARA CLUSTERING DE CARRERAS ===\n")

career_matrix_scaled <- scale(career_matrix)
k_range <- 2:10
silhouette_scores_careers <- numeric(length(k_range))

set.seed(123)
for (i in seq_along(k_range)) {
  k <- k_range[i]
  km_temp <- kmeans(career_matrix_scaled, centers = k, nstart = 25, iter.max = 100)
  sil <- silhouette(km_temp$cluster, dist(career_matrix_scaled))
  silhouette_scores_careers[i] <- mean(sil[, 3])
  cat(sprintf("K = %d: Silhouette promedio = %.4f\n", k, silhouette_scores_careers[i]))
}

# Identificar K óptimo
optimal_k_careers <- k_range[which.max(silhouette_scores_careers)]
cat(sprintf("\n>>> K ÓPTIMO PARA CARRERAS: %d (silueta = %.4f)\n\n", 
            optimal_k_careers, max(silhouette_scores_careers)))

# Crear dataframe de resultados
career_silhouette_df <- data.frame(
  K = k_range,
  Silhouette_Promedio = silhouette_scores_careers
)

# Gráfico de silueta
career_silhouette_plot <- ggplot(career_silhouette_df, aes(x = K, y = Silhouette_Promedio)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_vline(xintercept = optimal_k_careers, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = optimal_k_careers, y = max(silhouette_scores_careers), 
           label = paste("K óptimo =", optimal_k_careers),
           vjust = -0.5, hjust = 0.5, color = "red", size = 4, fontface = "bold") +
  labs(title = "Análisis de Silueta - Clustering de Carreras",
       subtitle = "K óptimo basado en el mayor promedio de silueta",
       x = "Número de Clusters (K)",
       y = "Promedio de Silueta") +
  scale_x_continuous(breaks = k_range) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

print(career_silhouette_plot)

# Usar K óptimo, pero limitar a máximo 6 (1 Other + 5 carreras estratégicas)
# Solo tenemos 5 carreras estratégicas, así que no podemos tener más de 6 clusters
k_careers <- min(optimal_k_careers, 6)
if (optimal_k_careers > 6) {
  cat(sprintf(">>> ADVERTENCIA: K óptimo es %d, pero limitando a 6 (máximo posible con 5 carreras estratégicas)\n", optimal_k_careers))
}
cat(sprintf("\n>>> Usando K = %d para clustering de carreras\n\n", k_careers))

set.seed(123)
km_careers <- kmeans(career_matrix_scaled, centers = k_careers, nstart = 25, iter.max = 100)

# Convertir centros normalizados a valores reales (des-normalizar)
career_means_norm <- apply(career_matrix, 2, mean, na.rm = TRUE)
career_sds_norm <- apply(career_matrix, 2, sd, na.rm = TRUE)
career_centers_real_norm <- t(apply(km_careers$centers, 1, function(row) {
  row * career_sds_norm + career_means_norm
}))
colnames(career_centers_real_norm) <- c("COMERCIAL", "ING_CIVIL", "DERECHO", "PSICOLOGIA", "PERIODISMO")

# Calcular también los porcentajes absolutos promedio por cluster (del total de matriculados)
# Esto ayuda a interpretar los clusters en el contexto real
career_data_with_cluster <- career_data %>%
  mutate(cluster_num = km_careers$cluster)

career_centers_abs <- career_data_with_cluster %>%
  group_by(cluster_num) %>%
  summarise(
    COMERCIAL_ABS = mean(PCNTJE_COMERCIAL_MATRI, na.rm = TRUE),
    ING_CIVIL_ABS = mean(PCNTJE_ING_CIVIL_MATRI, na.rm = TRUE),
    DERECHO_ABS = mean(PCNTJE_DERECHO_MATRI, na.rm = TRUE),
    PSICOLOGIA_ABS = mean(PCNTJE_PSICOLOGIA_MATRI, na.rm = TRUE),
    PERIODISMO_ABS = mean(PCNTJE_PERIODISMO_MATRI, na.rm = TRUE),
    SUMA_AVG_ESTRATEGICAS = mean(SUMA_PCT_CAREERAS_ESTRATEGICAS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cluster_num)

# Combinar valores normalizados y absolutos para visualización
career_centers_real <- as.data.frame(career_centers_real_norm) %>%
  mutate(CLUSTER_NUM = 1:nrow(.)) %>%
  left_join(career_centers_abs, by = c("CLUSTER_NUM" = "cluster_num"))

# Asignar nombres hardcodeados a los clusters de carreras (K=6)
cluster_names <- c(
  "1" = "Psicology",
  "2" = "Balanced",
  "3" = "Engineer",
  "4" = "Journalism",
  "5" = "Law",
  "6" = "Commercial"
  )
  
# Convertir a dataframe de asignaciones
cluster_assignments <- data.frame(
  RBD = career_data$RBD,
  cluster_num = km_careers$cluster,
  CLUSTER_NOMBRE = cluster_names[as.character(km_careers$cluster)],
  stringsAsFactors = FALSE
)

schools <- schools %>%
  left_join(cluster_assignments, by = "RBD") %>%
  # NO asignar "Other" a colegios BAJO_VOLUMEN - dejarlos como NA para excluirlos de la clusterización
  # NOTA: "Other" es el cluster que NO muestra preferencia clara (menor diferencia entre 1era y 2da carrera)
  # Solo asignar "Other" como fallback a colegios que no tienen cluster asignado (casos raros/errores)
  mutate(CLUSTER_NOMBRE = if_else(
    ES_BAJO_VOLUMEN, 
    NA_character_,  # BAJO_VOLUMEN queda como NA, no como "Other" (evita distorsionar clusterización)
    if_else(is.na(CLUSTER_NOMBRE), "Other", CLUSTER_NOMBRE)
  ))

cat("Main career clusters assigned.\n")
cat("Colegios BAJO_VOLUMEN excluidos de clusterización:", sum(schools$ES_BAJO_VOLUMEN, na.rm = TRUE), "\n")

# Mostrar centros de clusters
cat("\n=== CENTROS DE CLUSTERS DE CARRERAS ===\n")

career_centers_df <- career_centers_real %>%
  mutate(CLUSTER_NOMBRE = cluster_names[as.character(CLUSTER_NUM)])

cat("\n--- VALORES NORMALIZADOS (suman ~100% entre las 5 carreras) ---\n")
print(career_centers_df %>% 
      select(CLUSTER_NUM, CLUSTER_NOMBRE, COMERCIAL, ING_CIVIL, DERECHO, PSICOLOGIA, PERIODISMO) %>%
      mutate(SUMA_CHECK = round(COMERCIAL + ING_CIVIL + DERECHO + PSICOLOGIA + PERIODISMO, 2)) %>%
      arrange(CLUSTER_NUM))

cat("\n--- VALORES ABSOLUTOS (% del total de matriculados) ---\n")
print(career_centers_df %>% 
      select(CLUSTER_NUM, CLUSTER_NOMBRE, 
             COMERCIAL_ABS, ING_CIVIL_ABS, DERECHO_ABS, PSICOLOGIA_ABS, PERIODISMO_ABS,
             SUMA_AVG_ESTRATEGICAS) %>%
      rename(COMERCIAL = COMERCIAL_ABS, ING_CIVIL = ING_CIVIL_ABS, DERECHO = DERECHO_ABS,
             PSICOLOGIA = PSICOLOGIA_ABS, PERIODISMO = PERIODISMO_ABS,
             SUMA_CAREERAS_ESTRATEGICAS = SUMA_AVG_ESTRATEGICAS) %>%
      arrange(CLUSTER_NUM))

run_competition_clustering <- function(df, cols, centers, label) {
  # Convertir a data.frame si es data.table para evitar problemas de sintaxis
  if (inherits(df, "data.table")) {
    df <- as.data.frame(df)
  }
  
  available <- cols[cols %in% names(df)]
  if (length(available) != length(cols)) {
    cat("Skipping", label, "- missing columns:", setdiff(cols, available), "\n")
    return(list(assignment = rep(NA_integer_, nrow(df)), centers = NULL, centers_real = NULL))
  }
  complete_rows <- complete.cases(df[, available, drop = FALSE])
  data_matrix <- as.matrix(df[complete_rows, available, drop = FALSE])
  data_matrix_scaled <- scale(data_matrix)
  set.seed(123)
  km <- kmeans(data_matrix_scaled, centers = centers, nstart = 25)
  
  # Crear assignment del tamaño del dataframe de entrada (df)
  assignment <- rep(NA_integer_, nrow(df))
  assignment[which(complete_rows)] <- km$cluster
  
  # Convertir centros a valores reales
  means <- apply(data_matrix, 2, mean, na.rm = TRUE)
  sds <- apply(data_matrix, 2, sd, na.rm = TRUE)
  centers_real <- t(apply(km$centers, 1, function(row) {
    row * sds + means
  }))
  colnames(centers_real) <- available
  
  return(list(assignment = assignment, centers = km$centers, centers_real = centers_real))
}

# =============================================================================
# CLUSTERING DE UNIVERSIDADES CON DATOS FILTRADOS (UAI, COMP1, COMP2, OTHERS)
# =============================================================================
# Usar porcentajes FILTRADOS para clusterización de universidades
# Incluir OTHERS como cuarta categoría y normalizar las 4 para que sumen 100%

# Asegurar que las columnas filtradas necesarias existan y sean numéricas
university_cols_filtered <- c("PCNTJE_MATRI_UAI_FILTRADO", "PCNTJE_COMPT1_MATRI_FILTRADO", "PCNTJE_COMPT2_MATRI_FILTRADO")
schools <- numericize(schools, university_cols_filtered)

# Calcular PCNTJE_MATRI_OTHERS_FILTRADO y preparar datos para clustering
schools_for_clustering <- schools %>%
  filter(!ES_BAJO_VOLUMEN) %>%
  mutate(
    # Calcular OTHERS filtrado como resto
    PCNTJE_MATRI_OTHERS_FILTRADO = pmax(0, 100 - 
      coalesce(PCNTJE_MATRI_UAI_FILTRADO, 0) - 
      coalesce(PCNTJE_COMPT1_MATRI_FILTRADO, 0) - 
      coalesce(PCNTJE_COMPT2_MATRI_FILTRADO, 0), na.rm = TRUE),
    # Calcular suma de porcentajes de las 4 categorías (UAI, COMP1, COMP2, OTHERS)
    SUMA_PCT_UNIVERSIDADES_FILTRADO = coalesce(PCNTJE_MATRI_UAI_FILTRADO, 0) +
                                      coalesce(PCNTJE_COMPT1_MATRI_FILTRADO, 0) +
                                      coalesce(PCNTJE_COMPT2_MATRI_FILTRADO, 0) +
                                      coalesce(PCNTJE_MATRI_OTHERS_FILTRADO, 0)
  ) %>%
  # Excluir colegios sin datos válidos
  filter(SUMA_PCT_UNIVERSIDADES_FILTRADO > 0) %>%
  # Normalizar las 4 categorías para que sumen 100% entre ellas
  mutate(
    PCNTJE_MATRI_UAI_FILTRADO_NORM = if_else(SUMA_PCT_UNIVERSIDADES_FILTRADO > 0,
                                              (coalesce(PCNTJE_MATRI_UAI_FILTRADO, 0) / SUMA_PCT_UNIVERSIDADES_FILTRADO) * 100,
                                              0),
    PCNTJE_COMPT1_MATRI_FILTRADO_NORM = if_else(SUMA_PCT_UNIVERSIDADES_FILTRADO > 0,
                                                 (coalesce(PCNTJE_COMPT1_MATRI_FILTRADO, 0) / SUMA_PCT_UNIVERSIDADES_FILTRADO) * 100,
                                                 0),
    PCNTJE_COMPT2_MATRI_FILTRADO_NORM = if_else(SUMA_PCT_UNIVERSIDADES_FILTRADO > 0,
                                                 (coalesce(PCNTJE_COMPT2_MATRI_FILTRADO, 0) / SUMA_PCT_UNIVERSIDADES_FILTRADO) * 100,
                                                 0),
    PCNTJE_MATRI_OTHERS_FILTRADO_NORM = if_else(SUMA_PCT_UNIVERSIDADES_FILTRADO > 0,
                                                 (coalesce(PCNTJE_MATRI_OTHERS_FILTRADO, 0) / SUMA_PCT_UNIVERSIDADES_FILTRADO) * 100,
                                                 0)
  ) %>%
  select(RBD, all_of(university_cols_filtered), "PCNTJE_MATRI_OTHERS_FILTRADO",
         paste0(university_cols_filtered, "_NORM"), "PCNTJE_MATRI_OTHERS_FILTRADO_NORM",
         SUMA_PCT_UNIVERSIDADES_FILTRADO) %>%
  drop_na(any_of(university_cols_filtered))

# Usar valores normalizados de las 4 categorías (UAI, COMP1, COMP2, OTHERS) para clusterización (suman 100%)
university_cols_norm_filtered <- c(paste0(university_cols_filtered, "_NORM"), "PCNTJE_MATRI_OTHERS_FILTRADO_NORM")
university_matrix <- schools_for_clustering %>%
  select(all_of(university_cols_norm_filtered))

# =============================================================================
# ANÁLISIS DE SILUETA PARA DETERMINAR K ÓPTIMO - UNIVERSIDADES (DATOS FILTRADOS)
# =============================================================================
cat("\n=== ANÁLISIS DE SILUETA PARA CLUSTERING DE UNIVERSIDADES (DATOS FILTRADOS) ===\n")
cat("Usando porcentajes FILTRADOS: UAI, COMP1, COMP2, OTHERS (normalizados a 100%)\n\n")

university_matrix_scaled <- scale(university_matrix)
k_range <- 2:10
silhouette_scores_universities <- numeric(length(k_range))

set.seed(123)
for (i in seq_along(k_range)) {
  k <- k_range[i]
  km_temp <- kmeans(university_matrix_scaled, centers = k, nstart = 25, iter.max = 100)
  sil <- silhouette(km_temp$cluster, dist(university_matrix_scaled))
  silhouette_scores_universities[i] <- mean(sil[, 3])
  cat(sprintf("K = %d: Silhouette promedio = %.4f\n", k, silhouette_scores_universities[i]))
}

# Identificar K óptimo
optimal_k_universities <- k_range[which.max(silhouette_scores_universities)]
cat(sprintf("\n>>> K ÓPTIMO PARA UNIVERSIDADES (FILTRADO): %d (silueta = %.4f)\n\n", 
            optimal_k_universities, max(silhouette_scores_universities)))

# Crear dataframe de resultados
university_silhouette_df <- data.frame(
  K = k_range,
  Silhouette_Promedio = silhouette_scores_universities
)

# Gráfico de silueta
university_silhouette_plot <- ggplot(university_silhouette_df, aes(x = K, y = Silhouette_Promedio)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  geom_vline(xintercept = optimal_k_universities, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = optimal_k_universities, y = max(silhouette_scores_universities), 
           label = paste("K óptimo =", optimal_k_universities),
           vjust = -0.5, hjust = 0.5, color = "red", size = 4, fontface = "bold") +
  labs(title = "Análisis de Silueta - Clustering de Universidades (Datos Filtrados)",
       subtitle = paste("K óptimo =", optimal_k_universities, "| Silueta =", round(max(silhouette_scores_universities), 4)),
       x = "Número de Clusters (K)",
       y = "Promedio de Silueta") +
  scale_x_continuous(breaks = k_range) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

print(university_silhouette_plot)

# Usar K = 7 según solicitud del usuario (luego se fusionarán clusters 4 y 7 para tener 6 finales)
k_universities <- 7
cat(sprintf("\n>>> K ÓPTIMO según silhouette: %d (silueta = %.4f)\n", optimal_k_universities, max(silhouette_scores_universities)))
cat(sprintf(">>> Usando K = %d para clustering de universidades con datos filtrados (según solicitud)\n\n", k_universities))

set.seed(123)
km_universities <- kmeans(university_matrix_scaled, centers = k_universities, nstart = 25, iter.max = 100)

# Asignar clusters originales
university_data_with_cluster <- schools_for_clustering %>%
  mutate(cluster_num_original = km_universities$cluster)

# =============================================================================
# FUSIONAR CLUSTERS 4 Y 7 EN UN SOLO CLUSTER
# =============================================================================
cat("\n=== FUSIONANDO CLUSTERS 4 Y 7 ===\n")
cat("Cluster original 4 y 7 se fusionarán en un solo cluster: COMP1_COMP2\n\n")

# Mapear clusters originales a clusters finales (6 clusters total)
# Original 1 -> Final 1: OTHERS_COMP1_LOW
# Original 2 -> Final 2: UAI
# Original 3 -> Final 3: COMP1
# Original 4 -> Final 4: COMP1_COMP2 (fusionado)
# Original 5 -> Final 5: UAI2
# Original 6 -> Final 6: Others
# Original 7 -> Final 4: COMP1_COMP2 (fusionado)

university_data_with_cluster <- university_data_with_cluster %>%
  mutate(
    cluster_num_final = case_when(
      cluster_num_original == 1 ~ 1L,  # OTHERS_COMP1_LOW
      cluster_num_original == 2 ~ 2L,  # UAI
      cluster_num_original == 3 ~ 3L,  # COMP1
      cluster_num_original == 4 ~ 4L,  # COMP1_COMP2
      cluster_num_original == 5 ~ 5L,  # UAI2
      cluster_num_original == 6 ~ 6L,  # Others
      cluster_num_original == 7 ~ 4L,  # COMP1_COMP2 (fusionado con 4)
      TRUE ~ NA_integer_
    ),
    cluster_num = cluster_num_final
  )

# Nombres de clusters finales (6 clusters)
university_cluster_names_final <- c(
  "1" = "OTHERS_COMP1_LOW",
  "2" = "UAI",
  "3" = "COMP1",
  "4" = "COMP1_COMP2",
  "5" = "UAI2",
  "6" = "Others"
)

cat("Mapeo de clusters:\n")
cat("  Original 1 -> Final 1: OTHERS_COMP1_LOW\n")
cat("  Original 2 -> Final 2: UAI\n")
cat("  Original 3 -> Final 3: COMP1\n")
cat("  Original 4 -> Final 4: COMP1_COMP2\n")
cat("  Original 5 -> Final 5: UAI2\n")
cat("  Original 6 -> Final 6: Others\n")
cat("  Original 7 -> Final 4: COMP1_COMP2 (fusionado)\n\n")

# Recalcular cluster centers para los 6 clusters finales usando valores normalizados
cat("=== RECALCULANDO CLUSTER CENTERS PARA 6 CLUSTERS FINALES ===\n\n")

# Calcular centros promedio de los valores normalizados por cluster final
university_centers_final_norm <- university_data_with_cluster %>%
  group_by(cluster_num) %>%
  summarise(
    UAI = mean(PCNTJE_MATRI_UAI_FILTRADO_NORM, na.rm = TRUE),
    COMP1 = mean(PCNTJE_COMPT1_MATRI_FILTRADO_NORM, na.rm = TRUE),
    COMP2 = mean(PCNTJE_COMPT2_MATRI_FILTRADO_NORM, na.rm = TRUE),
    OTHERS = mean(PCNTJE_MATRI_OTHERS_FILTRADO_NORM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cluster_num) %>%
  # Asegurar que sumen 100%
  rowwise() %>%
  mutate(
    SUMA = UAI + COMP1 + COMP2 + OTHERS,
    UAI = (UAI / SUMA) * 100,
    COMP1 = (COMP1 / SUMA) * 100,
    COMP2 = (COMP2 / SUMA) * 100,
    OTHERS = (OTHERS / SUMA) * 100,
    SUMA_CHECK = UAI + COMP1 + COMP2 + OTHERS
  ) %>%
  ungroup()

# Calcular también los porcentajes absolutos promedio por cluster final
university_centers_final_abs <- university_data_with_cluster %>%
  group_by(cluster_num) %>%
  summarise(
    UAI_ABS = mean(PCNTJE_MATRI_UAI_FILTRADO, na.rm = TRUE),
    COMP1_ABS = mean(PCNTJE_COMPT1_MATRI_FILTRADO, na.rm = TRUE),
    COMP2_ABS = mean(PCNTJE_COMPT2_MATRI_FILTRADO, na.rm = TRUE),
    OTHERS_ABS = mean(PCNTJE_MATRI_OTHERS_FILTRADO, na.rm = TRUE),
    SUMA_AVG_FILTRADO = mean(SUMA_PCT_UNIVERSIDADES_FILTRADO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cluster_num)

# Combinar valores normalizados y absolutos
university_centers_final <- university_centers_final_norm %>%
  left_join(university_centers_final_abs, by = "cluster_num") %>%
  mutate(CLUSTER_NUM = cluster_num) %>%
  select(CLUSTER_NUM, UAI, COMP1, COMP2, OTHERS, UAI_ABS, COMP1_ABS, COMP2_ABS, OTHERS_ABS, SUMA_AVG_FILTRADO)

# Agregar número de colegios por cluster final
cluster_counts_final <- university_data_with_cluster %>%
  count(cluster_num, name = "N_COLEGIOS")

university_centers_final <- university_centers_final %>%
  left_join(cluster_counts_final, by = c("CLUSTER_NUM" = "cluster_num")) %>%
  mutate(
    N_COLEGIOS = coalesce(N_COLEGIOS, 0L),
    CLUSTER_UNIVERSIDAD_NOMBRE = university_cluster_names_final[as.character(CLUSTER_NUM)]
  )

# Convertir a dataframe de asignaciones con clusters finales
university_cluster_assignments <- data.frame(
  RBD = university_data_with_cluster$RBD,
  CLUSTER_UNIVERSIDAD_NUM = university_data_with_cluster$cluster_num,
  CLUSTER_UNIVERSIDAD_NOMBRE = university_cluster_names_final[as.character(university_data_with_cluster$cluster_num)],
  stringsAsFactors = FALSE
)

schools <- schools %>%
  left_join(university_cluster_assignments, by = "RBD")

cat("University clustering completed.\n")

cat("\n=== CENTROS DE CLUSTERS DE UNIVERSIDADES FINALES (6 CLUSTERS) ===\n")
cat("NOTA: La clusterización usa valores NORMALIZADOS (UAI + COMP1 + COMP2 + OTHERS suman 100%%).\n")
cat("Esto permite comparar la preferencia relativa entre las 4 categorías.\n\n")

cat("\n--- VALORES NORMALIZADOS (suman ~100%% entre las 4 categorías: UAI, COMP1, COMP2, OTHERS) ---\n")
cat("(Usados para clusterización - comparación relativa entre todas las categorías)\n")
print(university_centers_final %>% 
      select(CLUSTER_NUM, CLUSTER_UNIVERSIDAD_NOMBRE, N_COLEGIOS, UAI, COMP1, COMP2, OTHERS) %>%
      mutate(SUMA_CHECK = round(UAI + COMP1 + COMP2 + OTHERS, 2)) %>%
      arrange(CLUSTER_NUM))

cat("\n--- VALORES ABSOLUTOS (% del total de matriculados filtrados) ---\n")
cat("(Para interpretación contextual)\n")
print(university_centers_final %>% 
      select(CLUSTER_NUM, CLUSTER_UNIVERSIDAD_NOMBRE, N_COLEGIOS,
             UAI_ABS, COMP1_ABS, COMP2_ABS, OTHERS_ABS,
             SUMA_AVG_FILTRADO) %>%
      rename(UAI = UAI_ABS, COMP1 = COMP1_ABS, COMP2 = COMP2_ABS, OTHERS = OTHERS_ABS,
             SUMA_FILTRADO = SUMA_AVG_FILTRADO) %>%
      arrange(CLUSTER_NUM))

# Recrear schools_for_clustering para los otros clusterings de competencia (opcionales)
# Usar los datos originales sin normalizar para estos clusterings
schools_for_clustering <- schools %>%
  filter(!ES_BAJO_VOLUMEN)

# Mantener los otros clusterings de competencia (opcionales, para análisis)
# Necesitamos mapear los clusters de vuelta al dataframe completo usando RBD
result_post <- run_competition_clustering(
  schools_for_clustering,
  c("PCNTJE_POSTULANTES_UAI", "PCNTJE_POST_COMPETENCIA1", "PCNTJE_POST_COMPETENCIA2", "PTJE_PROM"),
  centers = 5,
  label = "POST (unfiltered)"
)
# Mapear clusters de vuelta a schools usando RBD
if (length(result_post$assignment) > 0 && sum(!is.na(result_post$assignment)) > 0) {
  schools_for_clustering_with_cluster <- schools_for_clustering %>%
    select(RBD) %>%
    mutate(cluster_competencia_post = as.integer(result_post$assignment))
  # Asegurar que schools sea data.frame antes del join
  if (inherits(schools, "data.table")) {
    schools <- as.data.frame(schools)
  }
  schools <- schools %>%
    left_join(schools_for_clustering_with_cluster, by = "RBD")
} else {
  schools$cluster_competencia_post <- NA_integer_
}

result_matri <- run_competition_clustering(
  schools_for_clustering,
  c("PCNTJE_MATRICULADOS_UAI", "PCNTJE_MATRI_COMPETENCIA1", "PCNTJE_MATRI_COMPETENCIA2", "PTJE_PROM"),
  centers = 4,
  label = "MATRI (unfiltered)"
)
if (length(result_matri$assignment) > 0 && sum(!is.na(result_matri$assignment)) > 0) {
  schools_for_clustering_with_cluster <- schools_for_clustering %>%
    select(RBD) %>%
    mutate(cluster_competencia_matri = as.integer(result_matri$assignment))
  if (inherits(schools, "data.table")) {
    schools <- as.data.frame(schools)
  }
  schools <- schools %>%
    left_join(schools_for_clustering_with_cluster, by = "RBD")
} else {
  schools$cluster_competencia_matri <- NA_integer_
}

result_post_filt <- run_competition_clustering(
  schools_for_clustering,
  c("PCNTJE_POST_UAI_FILTRADO", "PCNTJE_COMPT1_POST_FILTRADO", "PCNTJE_COMPT2_POST_FILTRADO", "PTJE_PROM"),
  centers = 5,
  label = "POST filtrado"
)
if (length(result_post_filt$assignment) > 0 && sum(!is.na(result_post_filt$assignment)) > 0) {
  schools_for_clustering_with_cluster <- schools_for_clustering %>%
    select(RBD) %>%
    mutate(cluster_competencia_post_filtrado = as.integer(result_post_filt$assignment))
  if (inherits(schools, "data.table")) {
    schools <- as.data.frame(schools)
  }
  schools <- schools %>%
    left_join(schools_for_clustering_with_cluster, by = "RBD")
} else {
  schools$cluster_competencia_post_filtrado <- NA_integer_
}

result_matri_filt <- run_competition_clustering(
  schools_for_clustering,
  c("PCNTJE_MATRI_UAI_FILTRADO", "PCNTJE_COMPT1_MATRI_FILTRADO", "PCNTJE_COMPT2_MATRI_FILTRADO", "PTJE_PROM"),
  centers = 5,
  label = "MATRI filtrado"
)
if (length(result_matri_filt$assignment) > 0 && sum(!is.na(result_matri_filt$assignment)) > 0) {
  schools_for_clustering_with_cluster <- schools_for_clustering %>%
    select(RBD) %>%
    mutate(cluster_competencia_matri_filtrado = as.integer(result_matri_filt$assignment))
  if (inherits(schools, "data.table")) {
    schools <- as.data.frame(schools)
  }
  schools <- schools %>%
    left_join(schools_for_clustering_with_cluster, by = "RBD")
} else {
  schools$cluster_competencia_matri_filtrado <- NA_integer_
}

cat("Competition clustering completed.\n")

# 3) Ajustar columnas “OTRAS” para que los porcentajes por carrera cuadren.
add_otras_columns <- function(df, career_prefix) {
  post_total <- paste0("PCNTJE_", career_prefix, "_POST")
  post_uai <- paste0("PCNTJE_", career_prefix, "_UAI_POST")
  post_c1 <- paste0("PCNTJE_", career_prefix, "_COMPT1_POST")
  post_c2 <- paste0("PCNTJE_", career_prefix, "_COMPT2_POST")
  post_otras <- paste0("PCNTJE_", career_prefix, "_OTRAS_POST")
  
  matri_total <- paste0("PCNTJE_", career_prefix, "_MATRI")
  matri_uai <- paste0("PCNTJE_", career_prefix, "_UAI_MATRI")
  matri_c1 <- paste0("PCNTJE_", career_prefix, "_COMPT1_MATRI")
  matri_c2 <- paste0("PCNTJE_", career_prefix, "_COMPT2_MATRI")
  matri_otras <- paste0("PCNTJE_", career_prefix, "_OTRAS_MATRI")
  
  if (all(c(post_total, post_uai, post_c1, post_c2) %in% names(df))) {
    df[[post_otras]] <- pmax(
      0,
      df[[post_total]] - df[[post_uai]] - df[[post_c1]] - df[[post_c2]]
    )
  }
  if (all(c(matri_total, matri_uai, matri_c1, matri_c2) %in% names(df))) {
    df[[matri_otras]] <- pmax(
      0,
      df[[matri_total]] - df[[matri_uai]] - df[[matri_c1]] - df[[matri_c2]]
    )
  }
  df
}

for (career in career_groups) {
  schools <- add_otras_columns(schools, career)
}

cat("OTRAS columns computed.\n")

# Ensure numeric counts for matriculation breakdowns and derive 'OTRAS' counts
count_cols <- c(
  "MATRICULADOS", "MATRICULADOS_UAI", "MATRI_COMPETENCIA1", "MATRI_COMPETENCIA2",
  "MATRI_OTHERS"
)
for (career in career_groups) {
  count_cols <- c(
    count_cols,
    paste0("MATRI_", career),
    paste0("MATRI_", career, "_UAI"),
    paste0("MATRI_", career, "_COMPT1"),
    paste0("MATRI_", career, "_COMPT2")
  )
}
schools <- numericize(schools, unique(count_cols))

for (career in career_groups) {
  total_col <- paste0("MATRI_", career)
  uai_col <- paste0("MATRI_", career, "_UAI")
  comp1_col <- paste0("MATRI_", career, "_COMPT1")
  comp2_col <- paste0("MATRI_", career, "_COMPT2")
  other_col <- paste0("MATRI_", career, "_OTRAS")
  schools[[other_col]] <- pmax(
    0,
    coalesce(schools[[total_col]], 0) -
      coalesce(schools[[uai_col]], 0) -
      coalesce(schools[[comp1_col]], 0) -
      coalesce(schools[[comp2_col]], 0)
  )
}

# Ensure auxiliary columns exist before recomputation
if (!"MATRI_OTHERS" %in% names(schools)) {
  schools$MATRI_OTHERS <- NA_real_
}
if (!"POST_OTHERS" %in% names(schools)) {
  schools$POST_OTHERS <- NA_real_
}

col_numeric_market <- c(
  "MATRICULADOS", "MATRICULADOS_UAI", "MATRI_COMPETENCIA1", "MATRI_COMPETENCIA2",
  "MATRI_OTHERS", "POSTULANTES", "POSTULANTES_UAI", "POST_COMPETENCIA1",
  "POST_COMPETENCIA2", "POST_OTHERS"
)
schools <- numericize(schools, col_numeric_market)

# 4) Recalcular market share y lift global por sede.
schools <- schools %>%
  mutate(
    MATRI_OTHERS = ifelse(is.na(MATRI_OTHERS),
                          pmax(MATRICULADOS - MATRICULADOS_UAI - MATRI_COMPETENCIA1 - MATRI_COMPETENCIA2, 0),
                          MATRI_OTHERS),
    POST_OTHERS = ifelse(is.na(POST_OTHERS),
                         pmax(POSTULANTES - POSTULANTES_UAI - POST_COMPETENCIA1 - POST_COMPETENCIA2, 0),
                         POST_OTHERS),
    TOTAL_MARKET_MATRI = MATRICULADOS_UAI + MATRI_COMPETENCIA1 + MATRI_COMPETENCIA2 + MATRI_OTHERS,
    TOTAL_MARKET_POST = POSTULANTES_UAI + POST_COMPETENCIA1 + POST_COMPETENCIA2 + POST_OTHERS,
    MARKET_SHARE_UAI_MATRI = if_else(TOTAL_MARKET_MATRI > 0,
                                     round((MATRICULADOS_UAI / TOTAL_MARKET_MATRI) * 100, 2), 0),
    MARKET_SHARE_COMP1_MATRI = if_else(TOTAL_MARKET_MATRI > 0,
                                       round((MATRI_COMPETENCIA1 / TOTAL_MARKET_MATRI) * 100, 2), 0),
    MARKET_SHARE_COMP2_MATRI = if_else(TOTAL_MARKET_MATRI > 0,
                                       round((MATRI_COMPETENCIA2 / TOTAL_MARKET_MATRI) * 100, 2), 0),
    MARKET_SHARE_OTHERS_MATRI = if_else(TOTAL_MARKET_MATRI > 0,
                                        round((MATRI_OTHERS / TOTAL_MARKET_MATRI) * 100, 2), 0),
    MARKET_SHARE_UAI_POST = if_else(TOTAL_MARKET_POST > 0,
                                    round((POSTULANTES_UAI / TOTAL_MARKET_POST) * 100, 2), 0),
    MARKET_SHARE_COMP1_POST = if_else(TOTAL_MARKET_POST > 0,
                                      round((POST_COMPETENCIA1 / TOTAL_MARKET_POST) * 100, 2), 0),
    MARKET_SHARE_COMP2_POST = if_else(TOTAL_MARKET_POST > 0,
                                      round((POST_COMPETENCIA2 / TOTAL_MARKET_POST) * 100, 2), 0),
    MARKET_SHARE_OTHERS_POST = if_else(TOTAL_MARKET_POST > 0,
                                       round((POST_OTHERS / TOTAL_MARKET_POST) * 100, 2), 0),
    LIFT_UAI_vs_COMP1 = if_else(MATRI_COMPETENCIA1 > 0,
                                round(MATRICULADOS_UAI / MATRI_COMPETENCIA1, 2), 0),
    LIFT_UAI_vs_COMP2 = if_else(MATRI_COMPETENCIA2 > 0,
                                round(MATRICULADOS_UAI / MATRI_COMPETENCIA2, 2), 0),
    LIFT_COMP1_vs_UAI = if_else(MATRICULADOS_UAI > 0,
                                round(MATRI_COMPETENCIA1 / MATRICULADOS_UAI, 2), 0),
    LIFT_COMP2_vs_UAI = if_else(MATRICULADOS_UAI > 0,
                                round(MATRI_COMPETENCIA2 / MATRICULADOS_UAI, 2), 0),
    POST_LIFT_UAI_vs_COMP1 = if_else(POST_COMPETENCIA1 > 0,
                                     round(POSTULANTES_UAI / POST_COMPETENCIA1, 2), 0),
    POST_LIFT_UAI_vs_COMP2 = if_else(POST_COMPETENCIA2 > 0,
                                     round(POSTULANTES_UAI / POST_COMPETENCIA2, 2), 0),
    POST_LIFT_COMP1_vs_UAI = if_else(POSTULANTES_UAI > 0,
                                     round(POST_COMPETENCIA1 / POSTULANTES_UAI, 2), 0),
    POST_LIFT_COMP2_vs_UAI = if_else(POSTULANTES_UAI > 0,
                                     round(POST_COMPETENCIA2 / POSTULANTES_UAI, 2), 0)
  )

cat("Market share and lift metrics computed.\n")

# 5) Asignar etiquetas de segmento competitivo final (UAI, COMP1, COMP2, etc.).
schools <- schools %>%
  mutate(
    MARKET_SHARE_SEGMENT = case_when(
      MARKET_SHARE_UAI_MATRI >= 50 ~ "UAI Dominant (≥50%)",
      MARKET_SHARE_UAI_MATRI >= 30 ~ "UAI Strong (30-49%)",
      MARKET_SHARE_UAI_MATRI >= 15 ~ "UAI Moderate (15-29%)",
      MARKET_SHARE_UAI_MATRI >= 5 ~ "UAI Weak (5-14%)",
      MARKET_SHARE_UAI_MATRI > 0 ~ "UAI Minimal (1-4%)",
      TRUE ~ "No UAI Presence"
    ),
    LIFT_SEGMENT_COMP1 = case_when(
      LIFT_UAI_vs_COMP1 >= 2.0 ~ "UAI Dominant (Lift ≥2.0)",
      LIFT_UAI_vs_COMP1 >= 1.5 ~ "UAI Strong (Lift 1.5-1.9)",
      LIFT_UAI_vs_COMP1 >= 1.0 ~ "UAI Advantage (Lift 1.0-1.4)",
      LIFT_UAI_vs_COMP1 >= 0.7 ~ "Balanced (Lift 0.7-0.9)",
      LIFT_UAI_vs_COMP1 >= 0.5 ~ "COMP1 Advantage (Lift 0.5-0.6)",
      LIFT_UAI_vs_COMP1 > 0 ~ "COMP1 Dominant (Lift <0.5)",
      TRUE ~ "No Competition"
    ),
    LIFT_SEGMENT_COMP2 = case_when(
      LIFT_UAI_vs_COMP2 >= 2.0 ~ "UAI Dominant (Lift ≥2.0)",
      LIFT_UAI_vs_COMP2 >= 1.5 ~ "UAI Strong (Lift 1.5-1.9)",
      LIFT_UAI_vs_COMP2 >= 1.0 ~ "UAI Advantage (Lift 1.0-1.4)",
      LIFT_UAI_vs_COMP2 >= 0.7 ~ "Balanced (Lift 0.7-0.9)",
      LIFT_UAI_vs_COMP2 >= 0.5 ~ "COMP2 Advantage (Lift 0.5-0.6)",
      LIFT_UAI_vs_COMP2 > 0 ~ "COMP2 Dominant (Lift <0.5)",
      TRUE ~ "No Competition"
    ),
    COMPETITIVE_POSITION = case_when(
      LIFT_UAI_vs_COMP1 >= 2 & LIFT_UAI_vs_COMP2 >= 2 ~ "UAI Dominant",
      LIFT_UAI_vs_COMP1 >= 1.5 | LIFT_UAI_vs_COMP2 >= 1.5 ~ "UAI Strong",
      LIFT_UAI_vs_COMP1 >= 1 | LIFT_UAI_vs_COMP2 >= 1 ~ "UAI Advantage",
      LIFT_UAI_vs_COMP1 >= 0.7 & LIFT_UAI_vs_COMP2 >= 0.7 ~ "Balanced",
      LIFT_UAI_vs_COMP1 >= 0.5 | LIFT_UAI_vs_COMP2 >= 0.5 ~ "Competitor Advantage",
      TRUE ~ "Competitor Dominant"
    )
  )

cat("Segmentation buckets assigned.\n")

schools <- schools %>%
  mutate(
    MARKET_SHARE_UAI_MATRI = as.numeric(MARKET_SHARE_UAI_MATRI),
    MARKET_SHARE_COMP1_MATRI = as.numeric(MARKET_SHARE_COMP1_MATRI),
    MARKET_SHARE_COMP2_MATRI = as.numeric(MARKET_SHARE_COMP2_MATRI),
    MARKET_SHARE_UAI_POST = as.numeric(MARKET_SHARE_UAI_POST),
    MARKET_SHARE_COMP1_POST = as.numeric(MARKET_SHARE_COMP1_POST),
    MARKET_SHARE_COMP2_POST = as.numeric(MARKET_SHARE_COMP2_POST),
    LIFT_UAI_vs_COMP1 = as.numeric(LIFT_UAI_vs_COMP1),
    LIFT_UAI_vs_COMP2 = as.numeric(LIFT_UAI_vs_COMP2),
    POST_LIFT_UAI_vs_COMP1 = as.numeric(POST_LIFT_UAI_vs_COMP1),
    POST_LIFT_UAI_vs_COMP2 = as.numeric(POST_LIFT_UAI_vs_COMP2),
    TOTAL_MARKET_MATRI = as.numeric(TOTAL_MARKET_MATRI),
    TOTAL_MARKET_POST = as.numeric(TOTAL_MARKET_POST),
    MAX_MARKET_SHARE_MATRI = pmax(
      MARKET_SHARE_UAI_MATRI,
      MARKET_SHARE_COMP1_MATRI,
      MARKET_SHARE_COMP2_MATRI,
      na.rm = TRUE
    ),
    MAX_MARKET_SHARE_POST = pmax(
      MARKET_SHARE_UAI_POST,
      MARKET_SHARE_COMP1_POST,
      MARKET_SHARE_COMP2_POST,
      na.rm = TRUE
    ),
    PLAYERS_ABOVE_TWO_MATRI = rowSums(cbind(
      MARKET_SHARE_UAI_MATRI >= 2,
      MARKET_SHARE_COMP1_MATRI >= 2,
      MARKET_SHARE_COMP2_MATRI >= 2
    ), na.rm = TRUE),
    PLAYERS_ABOVE_TWO_POST = rowSums(cbind(
      MARKET_SHARE_UAI_POST >= 2,
      MARKET_SHARE_COMP1_POST >= 2,
      MARKET_SHARE_COMP2_POST >= 2
    ), na.rm = TRUE),
    MARKET_WITH_DATA_MATRI = !is.na(TOTAL_MARKET_MATRI) & TOTAL_MARKET_MATRI >= 10,
    MARKET_WITH_DATA_POST = !is.na(TOTAL_MARKET_POST) & TOTAL_MARKET_POST >= 15,
    SEGMENTACION_FUENTE = case_when(
      MARKET_WITH_DATA_MATRI ~ "MATRICULA",
      !MARKET_WITH_DATA_MATRI & MARKET_WITH_DATA_POST ~ "POSTULACIONES",
      TRUE ~ "BAJO_VOLUMEN"
    ),
    DOMINANT_MATRI = case_when(
      MARKET_SHARE_UAI_MATRI == MAX_MARKET_SHARE_MATRI ~ "UAI",
      MARKET_SHARE_COMP1_MATRI == MAX_MARKET_SHARE_MATRI ~ "COMP1",
      MARKET_SHARE_COMP2_MATRI == MAX_MARKET_SHARE_MATRI ~ "COMP2",
      TRUE ~ "OTRAS"
    ),
    DOMINANT_POST = case_when(
      MARKET_SHARE_UAI_POST == MAX_MARKET_SHARE_POST ~ "UAI",
      MARKET_SHARE_COMP1_POST == MAX_MARKET_SHARE_POST ~ "COMP1",
      MARKET_SHARE_COMP2_POST == MAX_MARKET_SHARE_POST ~ "COMP2",
      TRUE ~ "OTRAS"
    ),
    PREFERENCIA_SEGMENTO = case_when(
      # Solo 4 segmentos: UAI, COMP1, COMP2, OTHERS (sin OPORTUNIDAD ni BAJO_VOLUMEN)
      SEGMENTACION_FUENTE == "MATRICULA" &
        MARKET_SHARE_UAI_MATRI > 15.5 ~ "UAI",
      SEGMENTACION_FUENTE == "MATRICULA" &
        MARKET_SHARE_UAI_MATRI > 6.75 &
        MARKET_SHARE_UAI_MATRI <= 15.5 &
        (LIFT_UAI_vs_COMP1 > 0.97 | LIFT_UAI_vs_COMP2 > 1.54) ~ "UAI",
      SEGMENTACION_FUENTE == "MATRICULA" &
        MARKET_SHARE_COMP1_MATRI > 27.7 ~ "COMP1",
      SEGMENTACION_FUENTE == "MATRICULA" &
        MARKET_SHARE_COMP1_MATRI > 13.5 &
        MARKET_SHARE_COMP1_MATRI <= 27.7 &
        LIFT_UAI_vs_COMP1 < 0.57 ~ "COMP1",
      SEGMENTACION_FUENTE == "MATRICULA" &
        MARKET_SHARE_COMP2_MATRI > 19.5 ~ "COMP2",
      SEGMENTACION_FUENTE == "MATRICULA" &
        MARKET_SHARE_COMP2_MATRI > 8.77 &
        MARKET_SHARE_COMP2_MATRI <= 19.5 &
        LIFT_UAI_vs_COMP2 < 0.83 ~ "COMP2",
      SEGMENTACION_FUENTE == "POSTULACIONES" &
        MARKET_SHARE_UAI_POST > 15.5 ~ "UAI",
      SEGMENTACION_FUENTE == "POSTULACIONES" &
        MARKET_SHARE_UAI_POST > 6.75 &
        MARKET_SHARE_UAI_POST <= 15.5 &
        (POST_LIFT_UAI_vs_COMP1 > 0.97 | POST_LIFT_UAI_vs_COMP2 > 1.54) ~ "UAI",
      SEGMENTACION_FUENTE == "POSTULACIONES" &
        MARKET_SHARE_COMP1_POST > 27.7 ~ "COMP1",
      SEGMENTACION_FUENTE == "POSTULACIONES" &
        MARKET_SHARE_COMP1_POST > 13.5 &
        MARKET_SHARE_COMP1_POST <= 27.7 &
        POST_LIFT_UAI_vs_COMP1 < 0.57 ~ "COMP1",
      SEGMENTACION_FUENTE == "POSTULACIONES" &
        MARKET_SHARE_COMP2_POST > 19.5 ~ "COMP2",
      SEGMENTACION_FUENTE == "POSTULACIONES" &
        MARKET_SHARE_COMP2_POST > 8.77 &
        MARKET_SHARE_COMP2_POST <= 19.5 &
        POST_LIFT_UAI_vs_COMP2 < 0.83 ~ "COMP2",
      # Si no cumple ninguna condición específica, usar el dominante o OTHERS
      SEGMENTACION_FUENTE == "MATRICULA" & DOMINANT_MATRI %in% c("UAI", "COMP1", "COMP2") ~ DOMINANT_MATRI,
      SEGMENTACION_FUENTE == "POSTULACIONES" & DOMINANT_POST %in% c("UAI", "COMP1", "COMP2") ~ DOMINANT_POST,
      SEGMENTACION_FUENTE == "MATRICULA" ~ "OTHERS",
      SEGMENTACION_FUENTE == "POSTULACIONES" ~ "OTHERS",
      TRUE ~ "BAJO_VOLUMEN"
    ),
    DOMINANT_BY_MARKET_SHARE = case_when(
      SEGMENTACION_FUENTE == "MATRICULA" ~ DOMINANT_MATRI,
      SEGMENTACION_FUENTE == "POSTULACIONES" ~ DOMINANT_POST,
      TRUE ~ "OTRAS"
    ),
    MAX_MARKET_SHARE = case_when(
      SEGMENTACION_FUENTE == "MATRICULA" ~ MAX_MARKET_SHARE_MATRI,
      SEGMENTACION_FUENTE == "POSTULACIONES" ~ MAX_MARKET_SHARE_POST,
      TRUE ~ NA_real_
    ),
    PLAYERS_ABOVE_TWO = case_when(
      SEGMENTACION_FUENTE == "MATRICULA" ~ PLAYERS_ABOVE_TWO_MATRI,
      SEGMENTACION_FUENTE == "POSTULACIONES" ~ PLAYERS_ABOVE_TWO_POST,
      TRUE ~ NA_real_
    ),
    MARKET_WITH_DATA = SEGMENTACION_FUENTE != "BAJO_VOLUMEN"
  )

segment_distribution <- schools %>%
  filter(!is.na(PREFERENCIA_SEGMENTO)) %>%
  count(PREFERENCIA_SEGMENTO, sort = TRUE) %>%
  mutate(Percentage = round((n / nrow(schools)) * 100, 1))
print(segment_distribution)

schools <- schools %>%
  mutate(
    PCNTJE_COMERCIAL_MATRI = as.numeric(PCNTJE_COMERCIAL_MATRI),
    PCNTJE_ING_CIVIL_MATRI = as.numeric(PCNTJE_ING_CIVIL_MATRI),
    PCNTJE_DERECHO_MATRI = as.numeric(PCNTJE_DERECHO_MATRI),
    PCNTJE_PSICOLOGIA_MATRI = as.numeric(PCNTJE_PSICOLOGIA_MATRI),
    PCNTJE_PERIODISMO_MATRI = as.numeric(PCNTJE_PERIODISMO_MATRI)
  ) %>%
  mutate(
    # Asegurar que los porcentajes sean numéricos y reemplazar NA con 0
    PCNTJE_COMERCIAL_MATRI = coalesce(as.numeric(PCNTJE_COMERCIAL_MATRI), 0),
    PCNTJE_ING_CIVIL_MATRI = coalesce(as.numeric(PCNTJE_ING_CIVIL_MATRI), 0),
    PCNTJE_DERECHO_MATRI = coalesce(as.numeric(PCNTJE_DERECHO_MATRI), 0),
    PCNTJE_PSICOLOGIA_MATRI = coalesce(as.numeric(PCNTJE_PSICOLOGIA_MATRI), 0),
    PCNTJE_PERIODISMO_MATRI = coalesce(as.numeric(PCNTJE_PERIODISMO_MATRI), 0),
    MAX_CAREER_PCT = pmax(
      PCNTJE_COMERCIAL_MATRI, PCNTJE_ING_CIVIL_MATRI, PCNTJE_DERECHO_MATRI,
      PCNTJE_PSICOLOGIA_MATRI, PCNTJE_PERIODISMO_MATRI,
      na.rm = TRUE
    ),
    CARRERA_DOMINANTE = case_when(
      !is.na(MAX_CAREER_PCT) & PCNTJE_COMERCIAL_MATRI == MAX_CAREER_PCT & MAX_CAREER_PCT > 0 ~ "Commercial",
      !is.na(MAX_CAREER_PCT) & PCNTJE_ING_CIVIL_MATRI == MAX_CAREER_PCT & MAX_CAREER_PCT > 0 ~ "Civil",
      !is.na(MAX_CAREER_PCT) & PCNTJE_DERECHO_MATRI == MAX_CAREER_PCT & MAX_CAREER_PCT > 0 ~ "Law",
      !is.na(MAX_CAREER_PCT) & PCNTJE_PSICOLOGIA_MATRI == MAX_CAREER_PCT & MAX_CAREER_PCT > 0 ~ "Psychology",
      !is.na(MAX_CAREER_PCT) & PCNTJE_PERIODISMO_MATRI == MAX_CAREER_PCT & MAX_CAREER_PCT > 0 ~ "Journalism",
      TRUE ~ "Other"  # Incluye casos donde MAX_CAREER_PCT es NA o 0
    )
  )

# Definir las 6 categorías de carrera siempre
career_categories <- c("Commercial", "Civil", "Law", "Psychology", "Journalism", "Other")
segment_categories <- c("UAI", "COMP1", "COMP2", "OTHERS")

composition_by_segment <- schools %>%
  filter(!is.na(PREFERENCIA_SEGMENTO)) %>%
  # Asegurar que CARRERA_DOMINANTE nunca sea NA
  mutate(
    CARRERA_DOMINANTE = if_else(is.na(CARRERA_DOMINANTE), "Other", CARRERA_DOMINANTE),
    # Reemplazar BAJO_VOLUMEN con OTHERS
    PREFERENCIA_SEGMENTO = if_else(PREFERENCIA_SEGMENTO == "BAJO_VOLUMEN", "OTHERS", PREFERENCIA_SEGMENTO)
  ) %>%
  filter(PREFERENCIA_SEGMENTO %in% segment_categories) %>%
  group_by(PREFERENCIA_SEGMENTO, CARRERA_DOMINANTE) %>%
  summarise(
    n_schools = n(),
    avg_commercial = round(mean(PCNTJE_COMERCIAL_MATRI, na.rm = TRUE), 2),
    avg_civil = round(mean(PCNTJE_ING_CIVIL_MATRI, na.rm = TRUE), 2),
    avg_law = round(mean(PCNTJE_DERECHO_MATRI, na.rm = TRUE), 2),
    avg_psychology = round(mean(PCNTJE_PSICOLOGIA_MATRI, na.rm = TRUE), 2),
    avg_journalism = round(mean(PCNTJE_PERIODISMO_MATRI, na.rm = TRUE), 2),
    avg_other = round(mean(pmax(0, 100 -
                                    PCNTJE_COMERCIAL_MATRI -
                                    PCNTJE_ING_CIVIL_MATRI -
                                    PCNTJE_DERECHO_MATRI -
                                    PCNTJE_PSICOLOGIA_MATRI -
                                    PCNTJE_PERIODISMO_MATRI), na.rm = TRUE), 2),
    avg_market_share_uai = round(mean(MARKET_SHARE_UAI_MATRI, na.rm = TRUE), 2),
    avg_market_share_comp1 = round(mean(MARKET_SHARE_COMP1_MATRI, na.rm = TRUE), 2),
    avg_market_share_comp2 = round(mean(MARKET_SHARE_COMP2_MATRI, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  # Crear todas las combinaciones posibles para asegurar las 6 carreras siempre
  complete(PREFERENCIA_SEGMENTO = segment_categories, 
           CARRERA_DOMINANTE = career_categories,
           fill = list(n_schools = 0, 
                      avg_commercial = 0, avg_civil = 0, avg_law = 0,
                      avg_psychology = 0, avg_journalism = 0, avg_other = 0,
                      avg_market_share_uai = 0, avg_market_share_comp1 = 0, avg_market_share_comp2 = 0)) %>%
  arrange(PREFERENCIA_SEGMENTO, CARRERA_DOMINANTE)

segment_summary <- schools %>%
  filter(!is.na(PREFERENCIA_SEGMENTO)) %>%
  group_by(PREFERENCIA_SEGMENTO) %>%
  summarise(
    total_schools = n(),
    commercial_schools = sum(CARRERA_DOMINANTE == "Commercial", na.rm = TRUE),
    civil_schools = sum(CARRERA_DOMINANTE == "Civil", na.rm = TRUE),
    law_schools = sum(CARRERA_DOMINANTE == "Law", na.rm = TRUE),
    psychology_schools = sum(CARRERA_DOMINANTE == "Psychology", na.rm = TRUE),
    journalism_schools = sum(CARRERA_DOMINANTE == "Journalism", na.rm = TRUE),
    other_schools = sum(CARRERA_DOMINANTE == "Other", na.rm = TRUE),
    avg_commercial_pct = round(mean(PCNTJE_COMERCIAL_MATRI, na.rm = TRUE), 2),
    avg_civil_pct = round(mean(PCNTJE_ING_CIVIL_MATRI, na.rm = TRUE), 2),
    avg_law_pct = round(mean(PCNTJE_DERECHO_MATRI, na.rm = TRUE), 2),
    avg_psychology_pct = round(mean(PCNTJE_PSICOLOGIA_MATRI, na.rm = TRUE), 2),
    avg_journalism_pct = round(mean(PCNTJE_PERIODISMO_MATRI, na.rm = TRUE), 2),
    avg_other_pct = round(mean(pmax(0, 100 -
                                    PCNTJE_COMERCIAL_MATRI -
                                    PCNTJE_ING_CIVIL_MATRI -
                                    PCNTJE_DERECHO_MATRI -
                                    PCNTJE_PSICOLOGIA_MATRI -
                                    PCNTJE_PERIODISMO_MATRI), na.rm = TRUE), 2),
    avg_uai_ms = round(mean(MARKET_SHARE_UAI_MATRI, na.rm = TRUE), 2),
    avg_comp1_ms = round(mean(MARKET_SHARE_COMP1_MATRI, na.rm = TRUE), 2),
    avg_comp2_ms = round(mean(MARKET_SHARE_COMP2_MATRI, na.rm = TRUE), 2),
    avg_lift_comp1 = round(mean(LIFT_UAI_vs_COMP1, na.rm = TRUE), 2),
    avg_lift_comp2 = round(mean(LIFT_UAI_vs_COMP2, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(total_schools))

cluster_competition_totals <- schools %>%
  # Excluir BAJO_VOLUMEN de los análisis de clusters
  filter(!ES_BAJO_VOLUMEN, !is.na(CLUSTER_NOMBRE)) %>%
  # Asegurar que CLUSTER_NOMBRE nunca sea NA (ya filtrado arriba, pero por seguridad)
  mutate(CLUSTER_NOMBRE = if_else(is.na(CLUSTER_NOMBRE), "Other", CLUSTER_NOMBRE)) %>%
  group_by(CLUSTER_NOMBRE) %>%
  summarise(
    MATRICULADOS_TOTALES = sum(MATRICULADOS, na.rm = TRUE),
    MATRICULADOS_UAI = sum(MATRICULADOS_UAI, na.rm = TRUE),
    MATRICULADOS_COMP1 = sum(MATRI_COMPETENCIA1, na.rm = TRUE),
    MATRICULADOS_COMP2 = sum(MATRI_COMPETENCIA2, na.rm = TRUE),
    MATRICULADOS_OTROS = sum(MATRI_OTHERS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_uai = round((MATRICULADOS_UAI / pmax(MATRICULADOS_TOTALES, 1)) * 100, 2),
    pct_comp1 = round((MATRICULADOS_COMP1 / pmax(MATRICULADOS_TOTALES, 1)) * 100, 2),
    pct_comp2 = round((MATRICULADOS_COMP2 / pmax(MATRICULADOS_TOTALES, 1)) * 100, 2),
    pct_otros = round((MATRICULADOS_OTROS / pmax(MATRICULADOS_TOTALES, 1)) * 100, 2)
  ) %>%
  arrange(desc(MATRICULADOS_TOTALES))

cluster_career_comp <- schools %>%
  # Excluir BAJO_VOLUMEN de los análisis de clusters
  filter(!ES_BAJO_VOLUMEN, !is.na(CLUSTER_NOMBRE)) %>%
  # Asegurar que CLUSTER_NOMBRE nunca sea NA (ya filtrado arriba, pero por seguridad)
  mutate(CLUSTER_NOMBRE = if_else(is.na(CLUSTER_NOMBRE), "Other", CLUSTER_NOMBRE)) %>%
  select(
    CLUSTER_NOMBRE,
    matches("^MATRI_(COMERCIAL|ING_CIVIL|DERECHO|PSICOLOGIA|PERIODISMO)_(UAI|COMPT1|COMPT2|OTRAS)$")
  ) %>%
  pivot_longer(
    cols = -CLUSTER_NOMBRE,
    names_to = c("CAREER", "COMPETIDOR"),
    names_pattern = "MATRI_(.*)_(UAI|COMPT1|COMPT2|OTRAS)",
    values_to = "MATRICULADOS"
  ) %>%
  mutate(
    CAREER = factor(CAREER, levels = career_groups),
    COMPETIDOR = recode(COMPETIDOR,
                        UAI = "UAI",
                        COMPT1 = "COMP1",
                        COMPT2 = "COMP2",
                        OTRAS = "OTROS")
  ) %>%
  group_by(CLUSTER_NOMBRE, CAREER, COMPETIDOR) %>%
  summarise(
    MATRICULADOS = sum(MATRICULADOS, na.rm = TRUE),
    .groups = "drop"
  )

cluster_career_totals <- schools %>%
  # Excluir BAJO_VOLUMEN de los análisis de clusters
  filter(!ES_BAJO_VOLUMEN, !is.na(CLUSTER_NOMBRE)) %>%
  # Asegurar que CLUSTER_NOMBRE nunca sea NA (ya filtrado arriba, pero por seguridad)
  mutate(CLUSTER_NOMBRE = if_else(is.na(CLUSTER_NOMBRE), "Other", CLUSTER_NOMBRE)) %>%
  select(CLUSTER_NOMBRE, all_of(paste0("MATRI_", career_groups))) %>%
  pivot_longer(
    cols = -CLUSTER_NOMBRE,
    names_to = "CAREER",
    values_to = "MATRICULADOS"
  ) %>%
  mutate(
    CAREER = gsub("^MATRI_", "", CAREER),
    CAREER = factor(CAREER, levels = career_groups),
    MATRICULADOS = coalesce(as.numeric(MATRICULADOS), 0)
  ) %>%
  group_by(CLUSTER_NOMBRE, CAREER) %>%
  summarise(MATRICULADOS = sum(MATRICULADOS, na.rm = TRUE), .groups = "drop")

# Agregar "Other" para completar la composición
cluster_career_other <- schools %>%
  # Excluir BAJO_VOLUMEN de los análisis de clusters
  filter(!ES_BAJO_VOLUMEN, !is.na(CLUSTER_NOMBRE)) %>%
  # Asegurar que CLUSTER_NOMBRE nunca sea NA (ya filtrado arriba, pero por seguridad)
  mutate(CLUSTER_NOMBRE = if_else(is.na(CLUSTER_NOMBRE), "Other", CLUSTER_NOMBRE)) %>%
  group_by(CLUSTER_NOMBRE) %>%
  summarise(
    MATRICULADOS = sum(
      pmax(0, coalesce(MATRICULADOS, 0) - 
        coalesce(MATRI_COMERCIAL, 0) - 
        coalesce(MATRI_ING_CIVIL, 0) - 
        coalesce(MATRI_DERECHO, 0) - 
        coalesce(MATRI_PSICOLOGIA, 0) - 
        coalesce(MATRI_PERIODISMO, 0)), 
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(CAREER = factor("OTHER", levels = c(career_groups, "OTHER")))

cluster_career_totals_with_other <- bind_rows(
  cluster_career_totals,
  cluster_career_other
)

# Asegurar que todos los clusters tengan las 6 categorías de carrera
all_clusters <- unique(cluster_career_totals_with_other$CLUSTER_NOMBRE)
all_careers <- c(career_groups, "OTHER")

cluster_career_totals_with_other <- cluster_career_totals_with_other %>%
  complete(CLUSTER_NOMBRE = all_clusters, CAREER = all_careers, fill = list(MATRICULADOS = 0))

cluster_career_share <- cluster_career_totals_with_other %>%
  group_by(CLUSTER_NOMBRE) %>%
  mutate(
    SHARE_PCT = round((MATRICULADOS / pmax(sum(MATRICULADOS), 1)) * 100, 2)
  ) %>%
  ungroup()

run_kmeans_thresholds <- function(values, actor_label, k = 4) {
  v <- values[values > 0 & !is.na(values)]
  v_unique <- unique(v)
  if (length(v_unique) < 2) {
    return(tibble())
  }
  k_use <- min(k, length(v_unique))
  km <- kmeans(matrix(v, ncol = 1), centers = k_use, nstart = 25)
  tibble(
    ACTOR = actor_label,
    CENTER = sort(as.numeric(km$centers)),
    K = k_use
  )
}

market_share_levels <- c("UAI", "COMP1", "COMP2")

market_share_thresholds <- bind_rows(
  run_kmeans_thresholds(schools$MARKET_SHARE_UAI_MATRI, "UAI"),
  run_kmeans_thresholds(schools$MARKET_SHARE_COMP1_MATRI, "COMP1"),
  run_kmeans_thresholds(schools$MARKET_SHARE_COMP2_MATRI, "COMP2")
) %>%
  mutate(ACTOR = factor(ACTOR, levels = market_share_levels))

lift_thresholds <- bind_rows(
  run_kmeans_thresholds(schools$LIFT_UAI_vs_COMP1, "UAI vs COMP1"),
  run_kmeans_thresholds(schools$LIFT_UAI_vs_COMP2, "UAI vs COMP2")
) %>%
  rename(LIFT_METRIC = ACTOR) %>%
  mutate(LIFT_METRIC = factor(LIFT_METRIC, levels = c("UAI vs COMP1", "UAI vs COMP2")))

# Santiago-specific summaries (Región Metropolitana)
santiago_schools <- schools %>%
  filter(
    REGION_RBD %in% c("13", 13) |
      grepl("METROPOLITANA", toupper(coalesce(REGION_GEO_NOMBRE, "")), fixed = TRUE) |
      grepl("SANTIAGO", toupper(coalesce(COMUNA, "")), fixed = TRUE)
  ) %>%
  mutate(MATRICULADOS = as.numeric(MATRICULADOS))

santiago_top_competencia <- santiago_schools %>%
  group_by(PREFERENCIA_SEGMENTO) %>%
  slice_max(order_by = MATRICULADOS, n = 10, with_ties = FALSE) %>%
  arrange(PREFERENCIA_SEGMENTO, desc(MATRICULADOS)) %>%
  ungroup() %>%
  select(
    PREFERENCIA_SEGMENTO,
    CLUSTER_NOMBRE,
    NOM_SEDE,
    RBD,
    COMUNA,
    MATRICULADOS,
    MARKET_SHARE_UAI_MATRI,
    MARKET_SHARE_COMP1_MATRI,
    MARKET_SHARE_COMP2_MATRI
  )

santiago_top_carrera <- santiago_schools %>%
  # Excluir BAJO_VOLUMEN de los análisis de clusters
  filter(!ES_BAJO_VOLUMEN, !is.na(CLUSTER_NOMBRE)) %>%
  group_by(CLUSTER_NOMBRE) %>%
  slice_max(order_by = MATRICULADOS, n = 10, with_ties = FALSE) %>%
  arrange(CLUSTER_NOMBRE, desc(MATRICULADOS)) %>%
  ungroup() %>%
  select(
    CLUSTER_NOMBRE,
    PREFERENCIA_SEGMENTO,
    NOM_SEDE,
    RBD,
    COMUNA,
    MATRICULADOS,
    MARKET_SHARE_UAI_MATRI,
    MARKET_SHARE_COMP1_MATRI,
    MARKET_SHARE_COMP2_MATRI
  )

market_share_long <- schools %>%
  select(
    SEDE_ID,
    MARKET_SHARE_UAI_MATRI,
    MARKET_SHARE_COMP1_MATRI,
    MARKET_SHARE_COMP2_MATRI
  ) %>%
  pivot_longer(
    cols = -SEDE_ID,
    names_to = "ACTOR",
    values_to = "MARKET_SHARE"
  ) %>%
  mutate(
    ACTOR = factor(
      recode(
        ACTOR,
        MARKET_SHARE_UAI_MATRI = "UAI",
        MARKET_SHARE_COMP1_MATRI = "COMP1",
        MARKET_SHARE_COMP2_MATRI = "COMP2"
      ),
      levels = market_share_levels
    )
  )

lift_long <- schools %>%
  select(SEDE_ID, LIFT_UAI_vs_COMP1, LIFT_UAI_vs_COMP2) %>%
  pivot_longer(
    cols = -SEDE_ID,
    names_to = "LIFT_METRIC",
    values_to = "LIFT"
  ) %>%
  mutate(
    LIFT_METRIC = factor(
      recode(
        LIFT_METRIC,
        LIFT_UAI_vs_COMP1 = "UAI vs COMP1",
        LIFT_UAI_vs_COMP2 = "UAI vs COMP2"
      ),
      levels = c("UAI vs COMP1", "UAI vs COMP2")
    )
  )

market_zero_counts <- market_share_long %>%
  summarise(
    zero_count = sum(is.na(MARKET_SHARE) | MARKET_SHARE <= 0),
    .by = ACTOR
  )

market_zero_labeller <- setNames(
  paste0(market_zero_counts$ACTOR, " (n0=", market_zero_counts$zero_count, ")"),
  market_zero_counts$ACTOR
)

lift_zero_counts <- lift_long %>%
  summarise(
    zero_count = sum(is.na(LIFT) | LIFT <= 0),
    .by = LIFT_METRIC
  )

lift_zero_labeller <- setNames(
  paste0(lift_zero_counts$LIFT_METRIC, " (n0=", lift_zero_counts$zero_count, ")"),
  lift_zero_counts$LIFT_METRIC
)

market_share_bins <- market_share_long %>%
  mutate(
    BIN = case_when(
      is.na(MARKET_SHARE) ~ "Faltante",
      MARKET_SHARE <= 0 ~ "0 exacto",
      TRUE ~ cut(
        MARKET_SHARE / 100,
        breaks = c(seq(0, 1, by = 0.1), Inf),
        labels = c(
          "0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5",
          "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0", ">1.0"
        ),
        include.lowest = TRUE,
        right = FALSE
      )
    ),
    BIN = if_else(is.na(BIN), "Sin asignar", as.character(BIN))
  ) %>%
  group_by(ACTOR, BIN) %>%
  summarise(
    n_colegios = n(),
    .groups = "drop"
  ) %>%
  group_by(ACTOR) %>%
  mutate(
    pct_dentro_actor = round((n_colegios / sum(n_colegios)) * 100, 2)
  ) %>%
  ungroup() %>%
  mutate(
    BIN = factor(
      BIN,
      levels = c(
        "Faltante", "0 exacto", "0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4",
        "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0", ">1.0", "Sin asignar"
      )
    )
  ) %>%
  arrange(ACTOR, BIN)

lift_bins <- lift_long %>%
  mutate(
    BIN = case_when(
      is.na(LIFT) ~ "Faltante",
      LIFT <= 0 ~ "0 exacto",
      TRUE ~ cut(
        LIFT,
        breaks = c(seq(0, ceiling(max(LIFT, na.rm = TRUE) + 0.1), by = 0.1)),
        include.lowest = TRUE,
        right = FALSE
      )
    ),
    BIN = if_else(is.na(BIN), "Sin asignar", as.character(BIN))
  ) %>%
  group_by(LIFT_METRIC, BIN) %>%
  summarise(
    n_colegios = n(),
    .groups = "drop"
  ) %>%
  group_by(LIFT_METRIC) %>%
  mutate(
    pct_dentro_metric = round((n_colegios / sum(n_colegios)) * 100, 2)
  ) %>%
  ungroup() %>%
  mutate(
    BIN = factor(BIN, levels = unique(BIN))
  ) %>%
  arrange(LIFT_METRIC, BIN)

cat("Segment summaries prepared.\n")

# 6) Exportar datasets enriquecidos y visualizaciones (si corresponde).
#    - CLUSTER_...WITH_NAMES_AND_LOCATIONS.csv: dataset maestro con métricas calculadas.
#    - segmento_distribucion.csv: conteo y porcentaje de segmentos competitivos.
#    - segmento_resumen_detallado.csv: estadísticas agregadas por segmento (lifts, market share, composición).
#    - segmento_composicion_carreras.csv: mezcla de carreras dominantes por segmento.
#    - cluster_matricula_competidores.csv: totales de matriculados por cluster y competidor (UAI/COMP1/COMP2/Otros).
#    - cluster_matricula_por_carrera.csv: matriculados por cluster, carrera y competidor.
#    - cluster_matricula_totales_por_carrera.csv: total de matriculados por cluster y carrera.
#    - cluster_carrera_porcentajes.csv: porcentaje que representa cada carrera dentro de cada cluster.
#    - Imagen `cluster_matricula_competidores.png`: barras apiladas con matrícula total por cluster y competidor.
#    - Imagen `cluster_matricula_carreras.png`: barras apiladas facetadas por carrera con detalle de competencia.
#    - Imagen `cluster_carrera_porcentajes.png`: composición porcentual de carreras dentro de cada cluster.
#    - Hist `hist_market_share_competidores.png` / `hist_lift_competencias.png`: distribuciones sin barra en 0 (conteo mostrado como anotación).
#    - Barras `market_share_tramos.png`, `lift_tramos.png`: frecuencias por tramos de 0.1 para market share (en proporción) y lift.
#    - Archivos `market_share_kmeans_thresholds.csv`, `lift_kmeans_thresholds.csv`: cortes sugeridos vía k-means 1D por market share y lift.
#    - Archivos `market_share_tramos.csv`, `lift_tramos.csv`: conteo y porcentaje en cada tramo granular.
#    - Archivos `santiago_top_competencia.csv`, `santiago_top_carreras.csv`: top sedes Región Metropolitana por segmento y preferencia.
output_cluster_year <- file.path(config$output_dir, sprintf("CLUSTER_%d_WITH_NAMES_AND_LOCATIONS.csv", pipeline_year))
output_cluster_latest <- file.path(config$output_dir, "CLUSTER_WITH_NAMES_AND_LOCATIONS.csv")
fwrite(schools, output_cluster_year, sep = ";")
fwrite(schools, output_cluster_latest, sep = ";")
cat("Saved cluster dataset:", output_cluster_year, "\n")

dist_output <- file.path(config$output_dir, "segmento_distribucion.csv")
summary_output <- file.path(config$output_dir, "segmento_resumen_detallado.csv")
composition_output <- file.path(config$output_dir, "segmento_composicion_carreras.csv")
cluster_comp_output <- file.path(config$output_dir, "cluster_matricula_competidores.csv")
cluster_career_output <- file.path(config$output_dir, "cluster_matricula_por_carrera.csv")
cluster_career_totals_output <- file.path(config$output_dir, "cluster_matricula_totales_por_carrera.csv")
cluster_career_share_output <- file.path(config$output_dir, "cluster_carrera_porcentajes.csv")
santiago_comp_output <- file.path(config$output_dir, "santiago_top_competencia.csv")
santiago_career_output <- file.path(config$output_dir, "santiago_top_carreras.csv")
market_share_thresholds_output <- file.path(config$output_dir, "market_share_kmeans_thresholds.csv")
lift_thresholds_output <- file.path(config$output_dir, "lift_kmeans_thresholds.csv")
market_share_bins_output <- file.path(config$output_dir, "market_share_tramos.csv")
lift_bins_output <- file.path(config$output_dir, "lift_tramos.csv")
fwrite(segment_distribution, dist_output, sep = ";")
fwrite(segment_summary, summary_output, sep = ";")
fwrite(composition_by_segment, composition_output, sep = ";")
fwrite(cluster_competition_totals, cluster_comp_output, sep = ";")
fwrite(cluster_career_comp, cluster_career_output, sep = ";")
fwrite(cluster_career_totals, cluster_career_totals_output, sep = ";")
fwrite(cluster_career_share, cluster_career_share_output, sep = ";")
fwrite(market_share_bins, market_share_bins_output, sep = ";")
fwrite(lift_bins, lift_bins_output, sep = ";")
if (nrow(santiago_top_competencia) > 0) {
  fwrite(santiago_top_competencia, santiago_comp_output, sep = ";")
}
if (nrow(santiago_top_carrera) > 0) {
  fwrite(santiago_top_carrera, santiago_career_output, sep = ";")
}
if (nrow(market_share_thresholds) > 0) {
  fwrite(market_share_thresholds, market_share_thresholds_output, sep = ";")
}
if (nrow(lift_thresholds) > 0) {
  fwrite(lift_thresholds, lift_thresholds_output, sep = ";")
}
cat("Saved segment summaries (CSV).\n")

if (config$export_plots) {
  cat("Rendering segment visuals...\n")
  plot_career_composition <- composition_by_segment %>%
    # Filtrar solo UAI, COMP1, COMP2, OTHERS (no BAJO_VOLUMEN)
    filter(PREFERENCIA_SEGMENTO %in% c("UAI", "COMP1", "COMP2", "OTHERS")) %>%
    group_by(PREFERENCIA_SEGMENTO) %>%
    mutate(percentage = round((n_schools / sum(n_schools)) * 100, 1)) %>%
    ggplot(aes(x = PREFERENCIA_SEGMENTO, y = percentage, fill = CARRERA_DOMINANTE)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "Commercial" = "#9b59b6",
      "Civil" = "#e74c3c",
      "Law" = "#f39c12",
      "Psychology" = "#1abc9c",
      "Journalism" = "#3498db",
      "Other" = "#95a5a6"
    )) +
    labs(
      title = "Composición por Carrera Dominante en Cada Segmento",
      subtitle = "Solo segmentos: UAI, COMP1, COMP2, OTHERS",
      x = "Segmento de Preferencia",
      y = "Porcentaje de Colegios (%)",
      fill = "Carrera Dominante"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    geom_text(aes(label = if_else(percentage > 3, paste0(percentage, "%"), "")),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white", fontface = "bold")
  
  print(plot_career_composition)
  
  plot_career_counts <- composition_by_segment %>%
    filter(PREFERENCIA_SEGMENTO %in% c("UAI", "COMP1", "COMP2", "OTHERS")) %>%
    ggplot(aes(x = CARRERA_DOMINANTE, y = n_schools, fill = PREFERENCIA_SEGMENTO)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c(
      "UAI" = "#27ae60",
      "COMP1" = "#f39c12",
      "COMP2" = "#e74c3c",
      "OTHERS" = "#95a5a6"
    )) +
    labs(
      title = "Número de Colegios por Carrera y Segmento",
      subtitle = "Solo segmentos: UAI, COMP1, COMP2, OTHERS",
      x = "Carrera Dominante",
      y = "Número de Colegios",
      fill = "Segmento"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    geom_text(aes(label = n_schools), position = position_dodge(width = 0.9),
              vjust = -0.3, size = 3)
  
  print(plot_career_counts)

  cluster_competition_totals_long <- cluster_competition_totals %>%
    select(CLUSTER_NOMBRE, MATRICULADOS_UAI, MATRICULADOS_COMP1, MATRICULADOS_COMP2, MATRICULADOS_OTROS) %>%
    pivot_longer(
      cols = -CLUSTER_NOMBRE,
      names_to = "COMPETIDOR",
      values_to = "MATRICULADOS"
    ) %>%
    mutate(
      COMPETIDOR = recode(COMPETIDOR,
                          MATRICULADOS_UAI = "UAI",
                          MATRICULADOS_COMP1 = "COMP1",
                          MATRICULADOS_COMP2 = "COMP2",
                          MATRICULADOS_OTROS = "OTROS"),
      CLUSTER_NOMBRE = factor(CLUSTER_NOMBRE, levels = sort(unique(CLUSTER_NOMBRE)))
    )

  plot_cluster_competitividad <- cluster_competition_totals_long %>%
    ggplot(aes(x = CLUSTER_NOMBRE, y = MATRICULADOS, fill = COMPETIDOR)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(
      "UAI" = "#27ae60",
      "COMP1" = "#f39c12",
      "COMP2" = "#e74c3c",
      "OTROS" = "#95a5a6"
    )) +
    labs(
      title = "Matrícula total por Cluster y Competidor",
      x = "Cluster",
      y = "Número de Matriculados",
      fill = "Competidor"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  print(plot_cluster_competitividad)

  cluster_career_comp_plot <- cluster_career_comp %>%
    mutate(
      CLUSTER_NOMBRE = factor(CLUSTER_NOMBRE, levels = sort(unique(CLUSTER_NOMBRE))),
      CAREER = factor(CAREER, levels = career_groups),
      COMPETIDOR = factor(COMPETIDOR, levels = c("UAI", "COMP1", "COMP2", "OTROS"))
    )

  plot_cluster_carrera_comp <- cluster_career_comp_plot %>%
    ggplot(aes(x = CLUSTER_NOMBRE, y = MATRICULADOS, fill = COMPETIDOR)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "UAI" = "#27ae60",
      "COMP1" = "#f39c12",
      "COMP2" = "#e74c3c",
      "OTROS" = "#95a5a6"
    )) +
    labs(
      title = "Matrícula por Cluster, Carrera y Competidor",
      x = "Cluster",
      y = "Número de Matriculados",
      fill = "Competidor"
    ) +
    facet_wrap(~CAREER, scales = "free_y") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  print(plot_cluster_carrera_comp)

  cluster_career_share_plot <- cluster_career_share %>%
    mutate(
      CLUSTER_NOMBRE = factor(CLUSTER_NOMBRE, levels = sort(unique(CLUSTER_NOMBRE))),
      CAREER = factor(CAREER, levels = c(career_groups, "OTHER")),
      CAREER_LABEL = recode(CAREER,
        "COMERCIAL" = "Commercial",
        "ING_CIVIL" = "Civil",
        "DERECHO" = "Law",
        "PSICOLOGIA" = "Psychology",
        "PERIODISMO" = "Journalism",
        "OTHER" = "Other"
      )
    ) %>%
    filter(!is.na(CAREER))

  plot_cluster_carrera_share <- cluster_career_share_plot %>%
    ggplot(aes(x = CLUSTER_NOMBRE, y = SHARE_PCT, fill = CAREER_LABEL)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "Commercial" = "#9b59b6",
      "Civil" = "#e74c3c",
      "Law" = "#f39c12",
      "Psychology" = "#1abc9c",
      "Journalism" = "#3498db",
      "Other" = "#95a5a6"
    )) +
    labs(
      title = "Composición porcentual de matrículas por carrera en cada cluster",
      subtitle = "Incluye las 5 carreras principales + Other",
      x = "Cluster",
      y = "Porcentaje de Matrículas (%)",
      fill = "Carrera"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    geom_text(
      aes(label = if_else(SHARE_PCT > 2, paste0(round(SHARE_PCT, 1), "%"), "")),
      position = position_stack(vjust = 0.5),
      size = 3,
      color = "white",
      fontface = "bold"
    )

  print(plot_cluster_carrera_share)

  market_zero_counts <- market_share_long %>%
    summarise(
      zero_count = sum(is.na(MARKET_SHARE) | MARKET_SHARE <= 0),
      .by = ACTOR
    )

  market_hist_data <- market_share_long %>%
    filter(!is.na(MARKET_SHARE), MARKET_SHARE > 0)

  plot_market_hist <- market_hist_data %>%
    ggplot(aes(x = MARKET_SHARE, fill = ACTOR)) +
    geom_histogram(alpha = 0.65, bins = 30, position = "identity") +
    facet_wrap(
      ~ACTOR,
      scales = "free_y",
      labeller = labeller(ACTOR = function(x) market_zero_labeller[as.character(x)])
    ) +
    scale_fill_manual(values = c("UAI" = "#27ae60", "COMP1" = "#f39c12", "COMP2" = "#e74c3c")) +
    geom_vline(
      data = market_share_thresholds,
      mapping = aes(xintercept = CENTER),
      color = "#2c3e50",
      linetype = "dashed",
      linewidth = 0.4
    ) +
    labs(
      title = "Distribución de Market Share por Competidor",
      x = "Market Share (%)",
      y = "Número de Colegios",
      fill = "Actor"
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      plot.margin = unit(c(1, 2, 1, 1), "lines")
    )

  print(plot_market_hist)

  lift_zero_counts <- lift_long %>%
    summarise(
      zero_count = sum(is.na(LIFT) | LIFT <= 0),
      .by = LIFT_METRIC
    )

  lift_hist_data <- lift_long %>%
    filter(!is.na(LIFT), LIFT > 0)

  plot_lift_hist <- lift_hist_data %>%
    ggplot(aes(x = LIFT, fill = LIFT_METRIC)) +
    geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
    scale_fill_manual(values = c("UAI vs COMP1" = "#2980b9", "UAI vs COMP2" = "#8e44ad")) +
    geom_vline(
      data = lift_thresholds,
      mapping = aes(xintercept = CENTER),
      color = "#2c3e50",
      linetype = "dashed",
      linewidth = 0.4
    ) +
    facet_wrap(
      ~LIFT_METRIC,
      scales = "free_y",
      labeller = labeller(LIFT_METRIC = function(x) lift_zero_labeller[as.character(x)])
    ) +
    labs(
      title = "Distribución de Lift (UAI vs Competidores)",
      x = "Lift",
      y = "Número de Colegios",
      fill = "Comparación"
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      plot.margin = unit(c(1, 2, 1, 1), "lines")
    )

  print(plot_lift_hist)
  
  plot_market_bins <- market_share_bins %>%
    filter(!BIN %in% c("Faltante", "Sin asignar")) %>%
    ggplot(aes(x = BIN, y = n_colegios, fill = ACTOR)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("UAI" = "#27ae60", "COMP1" = "#f39c12", "COMP2" = "#e74c3c")) +
    labs(
      title = "Distribución de market share por tramos (deciles)",
      x = "Tramo de market share (proporción)",
      y = "Número de colegios",
      fill = "Actor"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  print(plot_market_bins)

  plot_lift_bins <- lift_bins %>%
    filter(!BIN %in% c("Faltante", "Sin asignar")) %>%
    ggplot(aes(x = BIN, y = n_colegios, fill = LIFT_METRIC)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("UAI vs COMP1" = "#2980b9", "UAI vs COMP2" = "#8e44ad")) +
    labs(
      title = "Distribución de lift por tramos (0.1)",
      x = "Tramo de lift",
      y = "Número de colegios",
      fill = "Comparación"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  print(plot_lift_bins)

  plot_cluster_dist <- schools %>%
    # Excluir BAJO_VOLUMEN explícitamente - no deben aparecer en los análisis de clusters
    filter(!ES_BAJO_VOLUMEN, !is.na(PREFERENCIA_SEGMENTO), !is.na(CLUSTER_NOMBRE)) %>%
    # Reemplazar BAJO_VOLUMEN con OTHERS y filtrar solo UAI, COMP1, COMP2, OTHERS
    mutate(PREFERENCIA_SEGMENTO = if_else(PREFERENCIA_SEGMENTO == "BAJO_VOLUMEN", "OTHERS", PREFERENCIA_SEGMENTO)) %>%
    filter(PREFERENCIA_SEGMENTO %in% c("UAI", "COMP1", "COMP2", "OTHERS")) %>%
    count(PREFERENCIA_SEGMENTO, CLUSTER_NOMBRE) %>%
    group_by(PREFERENCIA_SEGMENTO) %>%
    mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
    ggplot(aes(x = PREFERENCIA_SEGMENTO, y = percentage, fill = CLUSTER_NOMBRE)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = "Distribución de Clusters de Carrera por Segmento Competitivo",
      subtitle = "Muestra qué clusters de colegios (carrera) componen cada segmento (UAI, COMP1, COMP2, OTHERS). BAJO_VOLUMEN excluido.",
      x = "Segmento",
      y = "Porcentaje de Colegios (%)",
      fill = "Cluster de Carrera"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    geom_text(aes(label = if_else(percentage > 2, paste0(percentage, "%"), "")),
              position = position_stack(vjust = 0.5),
              size = 2.5, color = "white", fontface = "bold")
  
  print(plot_cluster_dist)
}

cat("\n=== STEP 2 COMPLETE ===\n")
cat("Final dataset rows:", nrow(schools), "\n")
cat("Unique schools:", length(unique(schools$RBD)), "\n")

# =============================================================================
# EXPORTAR CSV CON CLUSTERS
# =============================================================================
# Crear dataset con todas las columnas del resumen más las columnas de clusters
# Renombrar las columnas de clusters para mantener nombres consistentes
# Eliminar columnas temporales, intermedias de geocodificación y duplicadas
resumen_colegios_clusters <- schools %>%
  mutate(
    CLUSTER_CARRERA = CLUSTER_NOMBRE,
    CLUSTER_UNIVERSIDAD = CLUSTER_UNIVERSIDAD_NOMBRE
  ) %>%
  # Asegurar que los valores NA se manejen apropiadamente
  mutate(
    CLUSTER_CARRERA = if_else(is.na(CLUSTER_CARRERA), NA_character_, CLUSTER_CARRERA),
    CLUSTER_UNIVERSIDAD = if_else(is.na(CLUSTER_UNIVERSIDAD), NA_character_, CLUSTER_UNIVERSIDAD)
  ) %>%
  # Eliminar columnas temporales (si existen)
  select(-any_of(c("MATRI_OTHERS_TEMP", "POST_OTHERS_TEMP", "TOTAL_MARKET_MATRI_TEMP", "TOTAL_MARKET_POST_TEMP"))) %>%
  # Eliminar columnas intermedias de geocodificación (si existen)
  select(-any_of(c("LATITUD_DIRECTORIO", "LONGITUD_DIRECTORIO", "COMUNA_DIRECTORIO", "REGION_DIRECTORIO",
                   "GEOCODE_NOMBRE", "COMUNA_EXTRA", "REGION_GEO_NOMBRE_EXTRA", "LATITUD_EXTRA", "LONGITUD_EXTRA",
                   "ESTADO_GEOCODIFICACION")))
  # NOTA: cluster_num se mantiene porque la visualización lo usa (lo renombra a CLUSTER_ID)

# Guardar el CSV
output_clusters_file <- file.path(config$output_dir, "CSV", "Resumen_colegios_clusters.csv")
fwrite(resumen_colegios_clusters, output_clusters_file, sep = ";")
cat("Saved clusters summary:", output_clusters_file, "\n")
cat("Total colegios en resumen:", nrow(resumen_colegios_clusters), "\n")
cat("Colegios con cluster de carrera:", sum(!is.na(resumen_colegios_clusters$CLUSTER_CARRERA)), "\n")
cat("Colegios con cluster de universidad:", sum(!is.na(resumen_colegios_clusters$CLUSTER_UNIVERSIDAD)), "\n")

