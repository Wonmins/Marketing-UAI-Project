# UAI Competitive Intelligence Pipelines

Este repositorio contiene dos pipelines en R diseñados para automatizar la preparación de datos de colegios y analizar su posicionamiento competitivo frente a la UAI y competidores directos. Además incluye utilidades para visualizar resultados, definir cortes de market share/lift y segmentar el mercado.

---

## 📋 Requisitos Previos

### Instalación de R
- R versión 4.0 o superior
- RStudio (recomendado) o cualquier editor de R

### Instalación de Librerías R

Ejecuta el siguiente código en R para instalar todas las dependencias:

```r
# Librerías para Pipeline Step 1
install.packages(c("data.table", "dplyr", "tidyr", "stringi"))

# Librerías para Pipeline Step 2
install.packages(c("ggplot2", "gridExtra", "grid", "factoextra", "cluster"))

# Librerías para Visualización (Quarto)
install.packages(c("tidyverse", "leaflet", "leaflet.extras", "leaflet.providers", 
                   "DT", "reactable", "RColorBrewer", "readr", "plotly", 
                   "classInt", "viridis", "htmlwidgets", "kableExtra"))

# Instalar Quarto (si no lo tienes)
# Visita: https://quarto.org/docs/get-started/
```

##LINKS
https://demre.cl/portales/datos-abiertos/datos-abiertos-matricula
https://datosabiertos.mineduc.cl/

---

## 📁 Estructura del Repositorio

```
.
├── Code/                                    # Scripts R del pipeline
│   ├── pipeline_step1_resumen_colegios.R   # Paso 1: resumen base por colegio/sede
│   ├── pipeline_step2_clustering_segmentation.R # Paso 2: clustering y segmentación
│   └── career_mapping_utils.r              # Utilidades para mapeo de carreras
│
├── CSV/                                     # Archivos de datos
│   ├── ArchivoD_Adm2024.csv                # Postulaciones (entrada)
│   ├── ArchivoMatr_Adm2024.csv             # Matrículas (entrada)
│   ├── ArchivoB_Adm2024.csv                # Inscritos (entrada)
│   ├── directorio.csv                       # Metadatos de colegios (entrada)
│   ├── Libro_CódigosADM2024_ArchivoD.csv   # Catálogo de carreras (entrada)
│   ├── career_mapping_categoria_detalle.csv # Mapping manual (entrada)
│   ├── colegios_coordenadas_completas.csv  # Coordenadas geográficas (entrada)
│   ├── RESUMEN_COLEGIOS.csv                # Salida del Paso 1
│   ├── Resumen_colegios_clusters.csv       # Salida final para visualización
│   └── CLUSTER_2024_WITH_NAMES_AND_LOCATIONS.csv # Salida del Paso 2
│
├── openstreetmap_integration.qmd           # Código fuente de la visualización
├── openstreetmap_integration.html          # Visualización compilada
├── README.md                               # Este archivo
└── .nojekyll                               # Configuración para GitHub Pages
```

---

## 1. Pipeline Step 1 – Resumen de Colegios

**Objetivo:** construir una base consolidada de colegios por año académico.

### Entradas (en `CSV/`):

1. `ArchivoD_AdmYYYY.csv`: postulaciones
2. `ArchivoMatr_AdmYYYY.csv`: matrículas
3. `ArchivoB_AdmYYYY.csv`: inscritos con datos socioeconómicos
4. `directorio.csv`: metadatos de colegios (RBD, nombres, regiones)
5. `Libro_CódigosADMYYYY_ArchivoD.csv`: catálogo oficial de carreras y universidades
6. `career_mapping_categoria_detalle.csv`: mapping manual de carreras estratégicas
7. `colegios_coordenadas_completas.csv` *(opcional)*: geocodificación por RBD

### Procesos principales:
- Limpieza y normalización de carreras (regex, ASCII, upper)
- Homologación de competidores (UAI/Competidor 1/Competidor 2)
- Agregaciones por colegio: postulantes, matriculados, market share filtrado, distribución GSE, métricas por carrera
- Identificación de multi-sedes (`SEDE_ID`, `NOM_SEDE`)
- Merge con coordenadas y direcciones

### Resultados (en `CSV/`):
- `RESUMEN_COLEGIOS_YYYY.csv` – base maestra por sede
- `RESUMEN_COLEGIOS_CON_COORDENADAS_YYYY.csv` – incluye lat/lon/dirección
- `RESUMEN_UAI_DETALLADO_POR_CARRERA_YYYY.csv` – detalle por carrera vs competidores

---

## 2. Pipeline Step 2 – Clustering y Segmentación

**Objetivo:** aplicar clustering, segmentación competitiva y generar tablas/gráficos para análisis.

### Entrada:
- `RESUMEN_COLEGIOS_YYYY.csv` generado en el paso 1 (ubicado en `CSV/`)

### Procesos principales:
- K-means sobre preferencias de matrícula por carrera (6 clusters)
- Recalculo de market shares, lifts y segmentación competitiva (UAI/COMP1/COMP2/Oportunidad/Bajo volumen)
- Fallback a postulaciones cuando la matrícula es <10; etiqueta `BAJO_VOLUMEN` si no hay datos suficientes
- Histogramas sin barra 0 (cero contado en títulos) + cortes sugeridos mediante k-means 1D
- Tramos de market share/lift en incrementos de 0.1
- Rankings para Región Metropolitana (top sedes por segmento y por cluster de carrera)
- Exportación de múltiples CSV e imágenes

### Resultados CSV (en `CSV/`):
| Archivo | Descripción |
| --- | --- |
| `Resumen_colegios_clusters.csv` | Dataset maestro con clustering, segmentación, market share, lift, ubicación (para visualización) |
| `CLUSTER_YYYY_WITH_NAMES_AND_LOCATIONS.csv` | Dataset maestro con clustering y segmentación |
| `segmento_distribucion.csv` | Conteo y % de colegios por segmento competitivo |
| `segmento_resumen_detallado.csv` | Market share, lift y métricas agregadas por segmento |
| `segmento_composicion_carreras.csv` | Composición de carreras dominantes por segmento competitivo |
| `cluster_matricula_competidores.csv` | Totales de matrícula UAI/COMP1/COMP2/Otros por cluster |
| `cluster_matricula_por_carrera.csv` | Matriculados por cluster, carrera y competidor |
| `cluster_matricula_totales_por_carrera.csv` | Totales de matrícula por cluster y carrera |
| `cluster_carrera_porcentajes.csv` | % de cada carrera dentro de cada cluster |
| `market_share_kmeans_thresholds.csv` | Cortes sugeridos (centros k-means 1D) para market share (>0) |
| `lift_kmeans_thresholds.csv` | Cortes sugeridos para lift (>0) |
| `market_share_tramos.csv` | Conteo y % de colegios por tramos de 0.1 en market share, incluyendo "0 exacto" |
| `lift_tramos.csv` | Conteo y % por tramos de 0.1 en lift |
| `santiago_top_competencia.csv` | Top 10 sedes de Santiago (Región Metropolitana) por segmento competitivo |
| `santiago_top_carreras.csv` | Top 10 sedes de Santiago por cluster de carrera |

### Resultados gráficos (PNG):
| Imagen | Descripción |
| --- | --- |
| `segmento_composicion_carreras.png` | Stacked bars: composición de carreras por segmento competitivo |
| `segmento_conteos_carreras.png` | Barras por carrera y segmento (conteo) |
| `segmento_distribucion_clusters.png` | Distribución de clusters de carrera por segmento competitivo |
| `cluster_matricula_competidores.png` | Matrícula total por cluster y competidor (UAI/Competidores/Otros) |
| `cluster_matricula_carreras.png` | Matrícula por cluster y carrera, desglosada por competidor |
| `cluster_carrera_porcentajes.png` | % de carreras dentro de cada cluster |
| `hist_market_share_competidores.png` | Histogramas de market share >0 por competidor. Títulos incluyen cantidad de `0 exacto` |
| `hist_lift_competencias.png` | Histogramas de lift >0; títulos muestran recuento de `0 exacto` |
| `market_share_tramos.png` | Barras comparativas por deciles de market share (0.1) |
| `lift_tramos.png` | Barras comparativas por tramos de 0.1 del lift |

---

## 3. Cómo Ejecutar los Pipelines

### Paso 1: Ejecutar Pipeline Step 1

```bash
# Desde la raíz del proyecto
Rscript Code/pipeline_step1_resumen_colegios.R 2024
```

### Paso 2: Ejecutar Pipeline Step 2

```bash
# Desde la raíz del proyecto
Rscript Code/pipeline_step2_clustering_segmentation.R 2024
```

**Nota:** Los argumentos son opcionales (por defecto `2024`), pero se recomienda especificar el año académico para cada corrida.

---

## 4. Visualización Interactiva

### Ver la Visualización

La visualización ya está compilada y disponible en:
- `openstreetmap_integration.html` - Abre este archivo en tu navegador

### Regenerar la Visualización

Si modificas el código fuente o los datos:

```bash
quarto render openstreetmap_integration.qmd
```

Esto generará un nuevo archivo `openstreetmap_integration.html` con todas las actualizaciones.

**Características de la Visualización:**
- 🗺️ Mapa interactivo con OpenStreetMap
- 📊 Tablas interactivas con filtros y búsqueda
- 📈 Análisis de clusters y segmentación
- 📱 Diseño responsive para móviles

---

## 5. Tips y Buenas Prácticas

1. **Mapping de carreras:** revisar y mantener actualizado `CSV/career_mapping_categoria_detalle.csv`. Cualquier cambio impacta ambos pipelines.

2. **Coordenadas:** si faltan lat/lon en `CSV/colegios_coordenadas_completas.csv`, el pipeline continúa pero deja columnas vacías.

3. **Datos faltantes:** sedes con <10 matrículas totales caen en `BAJO_VOLUMEN`. Si hay ≥15 postulaciones, se usa fallback con market share de postulaciones.

4. **Market share = 0:** se contabiliza por separado y aparece en los títulos de los histogramas ("n0=…").

5. **Extensibilidad:** los scripts están modularizados para agregar nuevos competidores, carreras o gráficos si es necesario.

6. **Rutas:** todos los scripts asumen que se ejecutan desde la raíz del proyecto. Los archivos de entrada deben estar en `CSV/` y las salidas también se generarán en `CSV/`.

---

