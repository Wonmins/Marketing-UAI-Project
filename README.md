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

### 📥 Obtención de Datos Fuente

Los archivos CSV fuente deben obtenerse de los siguientes portales oficiales:

- **DEMRE (Datos Abiertos):** [https://demre.cl/portales/datos-abiertos/datos-abiertos-matricula](https://demre.cl/portales/datos-abiertos/datos-abiertos-matricula)
- **Ministerio de Educación (Datos Abiertos):** [https://datosabiertos.mineduc.cl/](https://datosabiertos.mineduc.cl/)

### Entradas (CSVs fuente en `CSV/`):

1. `ArchivoD_AdmYYYY.csv`: postulaciones (fuente: DEMRE)
2. `ArchivoMatr_AdmYYYY.csv`: matrículas (fuente: DEMRE)
3. `ArchivoB_AdmYYYY.csv`: inscritos con datos socioeconómicos (fuente: DEMRE)
4. `directorio.csv`: metadatos de colegios (RBD, nombres, regiones) (fuente: MINEDUC)
5. `Libro_CódigosADMYYYY_ArchivoD.csv`: catálogo oficial de carreras y universidades (fuente: DEMRE)
6. `career_mapping_categoria_detalle.csv`: mapping manual de carreras estratégicas (incluido en el repositorio)
7. `colegios_coordenadas_completas.csv` *(opcional)*: coordenadas geográficas por RBD

### Procesos principales:
- Limpieza y normalización de carreras
- Homologación de competidores (UAI/Competidor 1/Competidor 2)
- Agregaciones por colegio: postulantes, matriculados, market share, distribución GSE, métricas por carrera
- Identificación de multi-sedes
- Merge con coordenadas geográficas

### Resultados (en `CSV/`):
- `RESUMEN_COLEGIOS_YYYY.csv` – base consolidada por sede (entrada para Paso 2)

---

## 2. Pipeline Step 2 – Clustering y Segmentación

**Objetivo:** aplicar clustering, segmentación competitiva y generar tablas/gráficos para análisis.

### Entrada:
- `RESUMEN_COLEGIOS_YYYY.csv` generado en el paso 1 (ubicado en `CSV/`)

### Procesos principales:
- K-means sobre preferencias de matrícula por carrera (6 clusters)
- Clustering de universidades (6 clusters)
- Cálculo de market shares y lifts
- Segmentación competitiva (UAI/COMP1/COMP2/OTHERS)
- Fallback a postulaciones cuando la matrícula es <10
- Generación de visualizaciones y reportes auxiliares

### Resultados (en `CSV/`):
- `Resumen_colegios_clusters.csv` – **Archivo principal**: dataset maestro con clustering, segmentación, market share, lift y ubicación (usado por la visualización)

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

2. **Coordenadas:** el archivo `CSV/colegios_coordenadas_completas.csv` es opcional. Si no existe, el pipeline continúa pero deja columnas de coordenadas vacías.

3. **Datos faltantes:** sedes con <10 matrículas totales se etiquetan como `BAJO_VOLUMEN`. Si hay ≥15 postulaciones, se usa fallback con market share de postulaciones.

4. **Archivo final:** `Resumen_colegios_clusters.csv` es el único archivo necesario para la visualización. Los demás CSV generados son auxiliares para análisis específicos.

5. **Rutas:** ejecuta los scripts desde la raíz del proyecto. Todos los CSVs (fuentes y resultados) deben estar en `CSV/`.

---

## 6. Contacto / Issues

Para preguntas o ajustes adicionales (ej. nuevos cortes, dashboard, filtros regionales), respaldar la última salida y ajustar directamente en los scripts. Mantener comentarios actualizados ayuda a facilitar la colaboración entre analistas y data scientists.

¡Feliz análisis!
