# Configurar mirror de CRAN (necesario para Posit Connect Cloud)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Lista de paquetes necesarios
packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "readxl",
  "openxlsx",
  "dplyr",
  "ggplot2",
  "pwr",
  "plotly",
  "tidyr",
  "ThesiStats",
  "officer",
  "flextable",
  "httr2",
  "commonmark",
  "WRS2",       # Dependencia de ThesiStats
  "psych",      # Dependencia de ThesiStats
  "MVN",        # Dependencia de ThesiStats
  "gridExtra",  # Para gráficos de normalidad multivariada
  "gtable"      # Para gráficos de normalidad multivariada
)

# Función para verificar e instalar paquetes faltantes
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    message("Instalando paquetes faltantes: ", paste(new_packages, collapse = ", "))
    # Solo instalar dependencias obligatorias (Imports, Depends, LinkingTo)
    # NO instalar Suggests para evitar paquetes pesados como Hmisc
    install.packages(new_packages, dependencies = c("Depends", "Imports", "LinkingTo"), repos = "https://cloud.r-project.org")
  }
}

# Verificar e instalar paquetes necesarios
install_if_missing(packages)

# Prevenir instalación de paquetes durante la ejecución de la app
# Esto es necesario porque algunos paquetes intentan instalar dependencias en tiempo de ejecución
# lo cual no está permitido en Posit Connect Cloud
.GlobalEnv$.original_install.packages <- install.packages
install.packages <- function(...) {
  warning("Intento de instalar paquetes durante la ejecución de la app. Por favor, agregue el paquete a la lista inicial.")
  return(invisible(NULL))
}

# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(pwr)
library(plotly)
library(tidyr)
library(ThesiStats)
library(officer)
library(flextable)
library(httr2)
library(commonmark)

# Función auxiliar para test de Mardia
mardia_test <- function(data) {
  library(psych)
  library(dplyr)
  
  A <- mardia(data, plot = FALSE)
  
  # Crear el data frame de resultados
  result <- data.frame(
    Test = c("Mardia Skewness", "Mardia Kurtosis"),
    Statistic = c(A$small.skew, A$kurtosis),
    p.value = c(A$p.small, A$p.kurt),
    Result = c(ifelse(A$p.small < 0.05, "NO", "YES"), ifelse(A$p.kurt < 0.05, "NO", "YES"))
  )
  
  # Ajustar primero los valores p y luego aplicar el formato condicional
  result$p.value <- round(result$p.value, 3)  # Redondear todos los valores de p
  
  # Aplicar la condición de formato especial después del redondeo
  result$p.value <- ifelse(result$p.value <= 0.001, "p < .001", as.character(result$p.value))
  
  return(result)
}

# Función para crear gráfico de normalidad multivariada con test de Mardia
Multivariate_plot <- function(data, xmin = 30, xmax = 40, ymin = 2, ymax = 7) {
  library(MVN)
  library(ggplot2)
  library(gridExtra)
  library(gtable)
  library(grid)
  
  Descrip_fiabilidad <- mardia_test(data) %>% as.matrix.data.frame()
  
  tt2 <- ttheme_default(
    core = list(bg_params = list(fill = c("#F2F3F4","#F2F3F4", "#F2F3F4"), col = NA),
                fg_params = list(fontface = 1)),
    colhead = list(fg_params = list(col = "black", fontface = c(1,1,1,1,1))),
    rowhead = list(fg_params = list(col = "black", fontface = 1)), base_size = 8)
  
  # Calcular la distancia de Mahalanobis
  distancias <- mahalanobis(data, colMeans(data), cov(data))
  
  # Calcular los cuantiles teóricos de la distribución chi-cuadrado
  teorico <- qchisq(ppoints(length(distancias)), df = ncol(data))
  
  # Crear un data frame para ggplot
  df1 <- data.frame(Teorico = teorico, Observado = sort(distancias))
  
  # Crear el gráfico QQ
  p <- ggplot(df1, aes(x = Observado, y = Teorico)) +
    geom_point(shape = 19, color = "gray20", size = 2) +
    geom_abline(slope = 1, intercept = 0, color = "red", size = 1.2) +
    theme_bw() +
    labs(title = "Q-Q Plot de Distancias de Mahalanobis", x = "Squared Mahalanobis Distance", y = "Chi-Square Quantile") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    annotation_custom(gridExtra::tableGrob(Descrip_fiabilidad, rows = NULL, theme = tt2) %>%
                        gtable::gtable_add_grob(.,
                                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                                t = 2, b = nrow(.), l = 1, r = ncol(.)) %>%
                        gtable::gtable_add_grob(.,
                                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                                t = 1, l = 1, r = ncol(.)),
                      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  return(p)
}

# Función para limpiar datos y asegurar encoding UTF-8 correcto
limpiar_datos_encoding <- function(datos) {
  # Convertir nombres de columnas a UTF-8 para evitar problemas con tildes y caracteres especiales
  names(datos) <- enc2utf8(names(datos))
  
  # Limpiar nombres de columnas: eliminar espacios extra y caracteres problemáticos
  names(datos) <- trimws(names(datos))
  
  # Validar que no haya columnas sin nombre
  if (any(is.na(names(datos)) | names(datos) == "" | names(datos) == "NA")) {
    # Asignar nombres automáticos a columnas sin nombre
    empty_names <- which(is.na(names(datos)) | names(datos) == "" | names(datos) == "NA")
    names(datos)[empty_names] <- paste0("Col_", empty_names)
    warning(paste("Se encontraron", length(empty_names), "columnas sin nombre. Se asignaron nombres automáticos."))
  }
  
  # Eliminar columnas completamente vacías
  cols_vacias <- sapply(datos, function(x) all(is.na(x)))
  if (any(cols_vacias)) {
    cols_eliminadas <- names(datos)[cols_vacias]
    datos <- datos[, !cols_vacias, drop = FALSE]
    warning(paste("Se eliminaron", sum(cols_vacias), "columnas vacías:", paste(cols_eliminadas, collapse = ", ")))
  }
  
  # Validar que queden datos después de la limpieza
  if (nrow(datos) == 0 || ncol(datos) == 0) {
    stop("ERROR: El archivo no contiene datos válidos después de la limpieza.")
  }
  
  return(datos)
}

# Función wrapper robusta para edad_stat
edad_stat_safe <- function(data, variable_name) {
  # Validar que los datos existan
  if (is.null(data) || nrow(data) == 0) {
    return("Error: No hay datos disponibles")
  }
  
  # Validar que la variable exista en los datos
  if (is.null(variable_name) || variable_name == "" || !variable_name %in% names(data)) {
    available_vars <- paste(names(data), collapse = ", ")
    return(paste0("Error: La variable '", variable_name,
                  "' no existe en los datos.\nVariables disponibles: ", available_vars))
  }
  
  # Extraer la columna de edad
  edad_data <- data[[variable_name]]
  
  # Validar que sea numérica
  if (!is.numeric(edad_data)) {
    return(paste0("Error: La variable '", variable_name,
                  "' no es numérica. Tipo: ", class(edad_data)[1]))
  }
  
  # Intentar usar edad_stat del paquete ThesiStats
  tryCatch({
    # Crear un data frame temporal con el nombre correcto para la función
    temp_data <- data
    
    # Si edad_stat requiere específicamente una columna llamada 'Edad',
    # crear una copia con ese nombre
    if (variable_name != "Edad") {
      temp_data$Edad <- data[[variable_name]]
    }
    
    # Llamar a edad_stat
    result <- ThesiStats::edad_stat(temp_data, variable_name)
    return(result)
    
  }, error = function(e) {
    # Si edad_stat falla, usar implementación alternativa
    edad_values <- na.omit(edad_data)
    
    if (length(edad_values) == 0) {
      return("Error: No hay valores válidos en la variable de edad")
    }
    
    # Calcular estadísticas manualmente
    stats_df <- data.frame(
      Estadístico = c("Media", "Mediana", "Desviación Estándar", "Mínimo", "Máximo", "N"),
      Valor = c(
        round(mean(edad_values, na.rm = TRUE), 2),
        round(median(edad_values, na.rm = TRUE), 2),
        round(sd(edad_values, na.rm = TRUE), 2),
        min(edad_values, na.rm = TRUE),
        max(edad_values, na.rm = TRUE),
        length(edad_values)
      )
    )
    
    return(stats_df)
  })
}

# Versión de la aplicaciónss
APP_VERSION <- "2.1.0"

# =============================================================================
# CONTRASEÑA DE ACCESO A LA APLICACIÓN
# =============================================================================
# IMPORTANTE: Cambia esta contraseña por una de tu preferencia
# La contraseña por defecto es: ThesiStats2025
# Para cambiarla, simplemente modifica el valor entre comillas
APP_PASSWORD <- "tiempodemejorar"

# Función para generar reporte Word
build_thesistats_report <- function(
    outfile = sprintf("Reporte_ThesiStats_%s.docx", Sys.Date()),
    participantes_texto = "No se proporcionó información de participantes.",
    tabla1 = NULL,        # Descriptivos + fiabilidad (df)
    tabla2 = NULL,        # Correlaciones (df)
    tabla3 = NULL,        # Comparaciones (df)
    fig_cor_jpg = NULL,   # Ruta JPG del mapa de calor (opcional)
    fig_box_jpg = NULL,   # Ruta JPG de boxplots (opcional)
    fig_mvnorm_jpg = NULL # Ruta JPG de normalidad multivariada (opcional)
){
  ft_opt <- function(df) autofit(theme_booktabs(flextable(df)))
  
  doc <- read_docx() |>
    body_add_par("Participantes", style = "heading 1") |>
    body_add_par(participantes_texto, style = "Normal") |>
    body_add_par("Análisis de datos", style = "heading 1") |>
    body_add_par(paste(
      "Los análisis se realizaron en RStudio (Posit, 2023). Se aplicó Shapiro–Wilk (Apéndice A),",
      "boxplots para atípicos (Apéndice B), y normalidad multivariada (Mardia, Q–Q; Apéndice C).",
      "Las asociaciones se estimaron con Pearson winsorizado (rw). Las comparaciones se realizaron",
      "con t de Welch y d de Cohen robusta. La fiabilidad se estimó con omega (ω)."
    ), style = "Normal") |>
    body_add_par("Resultados", style = "heading 1") |>
    body_add_par("Descriptivos y fiabilidad", style = "heading 2")
  
  if(!is.null(tabla1)) doc <- doc |>
    body_add_flextable(ft_opt(tabla1)) |>
    body_add_par("Tabla 1. Estadísticos descriptivos y fiabilidad (ω).", style = "Normal")
  
  doc <- doc |>
    body_add_par("Correlaciones", style = "heading 2")
  
  if(!is.null(tabla2)) doc <- doc |>
    body_add_flextable(ft_opt(tabla2)) |>
    body_add_par("Tabla 2. Matriz de correlaciones (rw).", style = "Normal")
  
  doc <- doc |>
    body_add_par("Comparaciones por grupo", style = "heading 2")
  if(!is.null(tabla3)) doc <- doc |>
    body_add_flextable(ft_opt(tabla3)) |>
    body_add_par("Tabla 3. Comparaciones por grupo (t de Welch, d robusta).", style = "Normal")
  
  doc <- doc |>
    body_add_par("Apéndices", style = "heading 1")
  
  if(!is.null(fig_box_jpg)) doc <- doc |>
    body_add_par("Apéndice B. Boxplots (600 dpi).", style = "heading 2") |>
    body_add_img(fig_box_jpg, width = 6.5, height = 4.2)
  if(!is.null(fig_mvnorm_jpg)) doc <- doc |>
    body_add_par("Apéndice C. Normalidad multivariada.", style = "heading 2") |>
    body_add_img(fig_mvnorm_jpg, width = 6.5, height = 4.2)
  
  # Referencias
  doc <- doc |>
    body_add_par("Referencias", style = "heading 1") |>
    body_add_par("Aggarwal, C. C. (2017). An introduction to outlier analysis. In C. C. Aggarwal, Outlier analysis (2nd ed., pp. 1–34). Springer. https://doi.org/10.1007/978-3-319-47578-3_1", style = "Normal") |>
    body_add_par("Champely, S. (2020). pwr: Basic functions for power analysis (Version 1.3-0) [R package]. https://CRAN.R-project.org/package=pwr", style = "Normal") |>
    body_add_par("Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum Associates.", style = "Normal") |>
    body_add_par("Mair, P., & Wilcox, R. (2020). Robust statistical methods in R using the WRS2 package. Behavior Research Methods, 52, 464–488. https://doi.org/10.3758/s13428-019-01246-w", style = "Normal") |>
    body_add_par("McDonald, R. P. (1999). Test theory: A unified treatment. Lawrence Erlbaum Associates.", style = "Normal") |>
    body_add_par("Posit team. (2023). RStudio: Integrated Development Environment for R (Version 2023) [Computer software]. Posit Software, PBC. https://posit.co", style = "Normal") |>
    body_add_par("Razali, N. M., & Wah, Y. B. (2011). Power comparisons of Shapiro–Wilk, Kolmogorov–Smirnov, Lilliefors and Anderson–Darling tests. Journal of Statistical Modeling and Analytics, 2(1), 21–33.", style = "Normal")
  
  print(doc, target = outfile)
  message("Reporte guardado en: ", normalizePath(outfile))
}

# Función para filtrar líneas de texto que son solo encabezados (sin items)
filter_text_headers <- function(text_data) {
  # Extraer items para identificar cuáles tienen items asociados
  extracted <- extract_items(text_data)
  
  # Crear vector para almacenar líneas filtradas
  filtered_lines <- character()
  
  # Recorrer las líneas del texto original
  for (line in text_data) {
    # Limpiar la línea
    clean_line <- trimws(line)
    
    # Si la línea está vacía, mantenerla
    if (clean_line == "") {
      filtered_lines <- c(filtered_lines, line)
      next
    }
    
    # Si la línea contiene ":", probablemente tiene items, mantenerla
    if (grepl(":", clean_line)) {
      filtered_lines <- c(filtered_lines, line)
    } else {
      # Si no contiene ":", verificar si tiene items asociados en extracted
      # Si no tiene items (es un encabezado), no la incluimos
      # Solo la incluimos si NO está en extracted o si tiene valores
      if (!(clean_line %in% names(extracted)) || length(extracted[[clean_line]]) > 0) {
        filtered_lines <- c(filtered_lines, line)
      }
    }
  }
  
  return(filtered_lines)
}

# Función para crear mapeo de dimensiones a variables padre
create_dimension_variable_map <- function(text_data) {
  # Extraer items usando la función del paquete ThesiStats
  extracted <- extract_items(text_data)
  
  # Identificar qué nombres son variables (tienen chr(0)) y cuáles son dimensiones
  variable_map <- list()
  current_variable <- NULL
  
  for (i in seq_along(extracted)) {
    item_name <- names(extracted)[i]
    item_value <- extracted[[i]]
    
    # Si el valor es chr(0) (vacío), es un nombre de variable
    if (length(item_value) == 0) {
      current_variable <- item_name
    } else if (!is.null(current_variable)) {
      # Si tenemos una variable actual y este item tiene valores, mapear dimensión a variable
      variable_map[[item_name]] <- current_variable
    }
  }
  
  return(variable_map)
}

# Función para filtrar correlaciones relevantes
filter_relevant_correlations <- function(cor_matrix, dim_var_map) {
  if (is.null(cor_matrix) || is.null(dim_var_map) || length(dim_var_map) == 0) {
    return(cor_matrix)
  }
  
  # Convertir a matriz si no lo es
  cor_matrix <- as.matrix(cor_matrix)
  
  # Obtener nombres de filas y columnas
  row_names <- rownames(cor_matrix)
  col_names <- colnames(cor_matrix)
  
  # Crear una lista para almacenar las correlaciones relevantes
  relevant_cors <- list()
  
  # Iterar sobre las combinaciones de variables
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      if (i < j) {  # Solo la parte superior de la matriz (evitar duplicados)
        var1 <- row_names[i]
        var2 <- col_names[j]
        
        # Obtener las variables padre
        parent1 <- dim_var_map[[var1]]
        parent2 <- dim_var_map[[var2]]
        
        # Si ambas dimensiones tienen variables padre diferentes, es relevante
        if (!is.null(parent1) && !is.null(parent2) && parent1 != parent2) {
          cor_value <- cor_matrix[i, j]
          relevant_cors[[paste(var1, "-", var2)]] <- list(
            var1 = var1,
            var2 = var2,
            parent1 = parent1,
            parent2 = parent2,
            correlation = cor_value
          )
        }
      }
    }
  }
  
  return(relevant_cors)
}

# Función para formatear correlaciones relevantes para el prompt de IA
format_relevant_correlations <- function(relevant_cors) {
  if (length(relevant_cors) == 0) {
    return("No se encontraron correlaciones entre diferentes variables.")
  }
  
  output <- "Correlaciones relevantes (entre dimensiones de diferentes variables):\n\n"
  
  for (cor_name in names(relevant_cors)) {
    cor_info <- relevant_cors[[cor_name]]
    output <- paste0(output,
                     sprintf("%s (%s) - %s (%s): r = %.3f\n",
                             cor_info$var1, cor_info$parent1,
                             cor_info$var2, cor_info$parent2,
                             cor_info$correlation))
  }
  
  output <- paste0(output, "\nNOTA: Solo describe las correlaciones listadas arriba. ",
                   "NO describas correlaciones entre dimensiones de la misma variable ",
                   "(ej: no describas correlaciones entre EDS_Dim1 y EDS_Dim2 ya que ",
                   "ambas pertenecen a la misma variable 'Dependencia emocional').")
  
  return(output)
}

# CSS personalizado para mejorar el diseño
css <- "
/* Importar fuente Google */
@import url(https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap);

/* Variables CSS - Paleta Elegante y Profesional */
:root {
  --primary-color: #2c3e50;
  --primary-dark: #1a252f;
  --primary-light: #34495e;
  --accent-color: #27ae60;
  --accent-dark: #229954;
  --accent-gold: #d4af37;
  --info-color: #5d6d7e;
  --danger-color: #c0392b;
  --bg-light: #f5f6fa;
  --bg-card: #ffffff;
  --text-primary: #2c3e50;
  --text-secondary: #7f8c8d;
  --text-light: #95a5a6;
  --border-color: #dfe6e9;
  --shadow-sm: 0 2px 4px 0 rgba(44, 62, 80, 0.08);
  --shadow-md: 0 4px 12px 0 rgba(44, 62, 80, 0.12);
  --shadow-lg: 0 8px 24px 0 rgba(44, 62, 80, 0.15);
  --sidebar-bg: #2c3e50;
  --sidebar-bg-dark: #1a252f;
}

/* Estilos globales */
body {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  background-color: var(--bg-light);
  color: var(--text-primary);
}

.content-wrapper {
  background-color: var(--bg-light);
}

/* Header personalizado */
.main-header {
  background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%) !important;
  border: none !important;
  box-shadow: var(--shadow-lg);
}

.main-header .navbar {
  background: transparent !important;
}

.main-header .logo {
  font-weight: 700;
  font-size: 1.5rem;
  letter-spacing: -0.5px;
  background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%) !important;
  color: #ffffff !important;
  border-right: 1px solid rgba(255, 255, 255, 0.1);
  padding: 15px 20px !important;
}

.main-header .logo:hover {
  background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%) !important;
}

/* Icono del logo */
.main-header .logo .fa,
.main-header .logo .fas {
  font-size: 1.6rem;
  margin-right: 10px;
  vertical-align: middle;
}

/* Texto del logo */
.main-header .logo span {
  font-size: 1.5rem;
  font-weight: 700;
  letter-spacing: -0.5px;
}

/* Sidebar mejorado */
.main-sidebar {
  background: linear-gradient(180deg, #2c3e50 0%, #1a252f 100%) !important;
  box-shadow: var(--shadow-lg);
}

.skin-blue .main-sidebar {
  background: linear-gradient(180deg, #2c3e50 0%, #1a252f 100%) !important;
}

.sidebar-menu > li > a {
  color: #bdc3c7 !important;
  border-left: 3px solid transparent;
  transition: all 0.3s ease;
  font-weight: 500;
}

.sidebar-menu > li > a:hover {
  background-color: rgba(52, 73, 94, 0.3) !important;
  border-left-color: #d4af37;
  color: #ffffff !important;
}

.sidebar-menu > li.active > a {
  background: linear-gradient(90deg, rgba(212, 175, 55, 0.15) 0%, transparent 100%) !important;
  border-left-color: #d4af37;
  color: #ffffff !important;
  font-weight: 600;
}

.sidebar-menu .header {
  color: #95a5a6 !important;
}

/* Boxes mejorados */
.box {
  border-radius: 12px;
  box-shadow: var(--shadow-md);
  border: 1px solid var(--border-color);
  overflow: hidden;
  transition: all 0.3s ease;
  background: var(--bg-card);
}

.box:hover {
  box-shadow: var(--shadow-lg);
  transform: translateY(-2px);
}

.box-header {
  background: linear-gradient(135deg, var(--primary-color) 0%, var(--primary-dark) 100%);
  color: white;
  padding: 15px 20px;
  border-bottom: none;
}

.box-header.with-border {
  border-bottom: none;
}

.box-title {
  font-size: 1.1rem;
  font-weight: 600;
  letter-spacing: -0.3px;
}

.box-body {
  padding: 20px;
}

.box.box-primary .box-header {
  background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%);
}

.box.box-success .box-header {
  background: linear-gradient(135deg, #27ae60 0%, #229954 100%);
}

.box.box-info .box-header {
  background: linear-gradient(135deg, #5d6d7e 0%, #4a5568 100%);
}

.box.box-warning .box-header {
  background: linear-gradient(135deg, #d4af37 0%, #b8941f 100%);
}

/* Botones de variables */
.variable-btn {
  margin: 4px;
  padding: 10px 16px;
  border: 2px solid var(--border-color);
  border-radius: 25px;
  background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
  color: var(--text-primary);
  cursor: pointer;
  display: inline-block;
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  font-weight: 500;
  font-size: 1.05rem;
  box-shadow: var(--shadow-sm);
}

.variable-btn:hover {
  background: linear-gradient(135deg, #f1f5f9 0%, #e2e8f0 100%);
  border-color: var(--primary-light);
  transform: translateY(-2px);
  box-shadow: var(--shadow-md);
}

.variable-btn.selected {
  background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%);
  color: white;
  border-color: #2c3e50;
  box-shadow: 0 4px 12px rgba(44, 62, 80, 0.3);
  font-weight: 600;
}

.variable-btn.selected:hover {
  transform: translateY(-2px) scale(1.02);
  box-shadow: 0 6px 16px rgba(44, 62, 80, 0.4);
}

.variable-container {
  border: 2px dashed var(--border-color);
  border-radius: 12px;
  padding: 15px;
  min-height: 80px;
  background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
  transition: all 0.3s ease;
}

.variable-container:hover {
  border-color: var(--primary-light);
  background: #ffffff;
}

.section-title {
  font-weight: 600;
  margin-bottom: 12px;
  color: var(--text-primary);
  font-size: 1.15rem;
  letter-spacing: -0.3px;
}

/* Botones mejorados */
.btn {
  border-radius: 8px;
  font-weight: 500;
  padding: 10px 20px;
  transition: all 0.3s ease;
  border: none;
  box-shadow: var(--shadow-sm);
  letter-spacing: -0.2px;
}

.btn:hover {
  transform: translateY(-2px);
  box-shadow: var(--shadow-md);
}

.btn-primary {
  background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%);
  color: white;
}

.btn-primary:hover {
  background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%);
  box-shadow: 0 4px 12px rgba(44, 62, 80, 0.3);
}

.btn-success {
  background: linear-gradient(135deg, #27ae60 0%, #229954 100%);
  color: white;
}

.btn-success:hover {
  background: linear-gradient(135deg, #229954 0%, #1e8449 100%);
  box-shadow: 0 4px 12px rgba(39, 174, 96, 0.3);
}

.btn-info {
  background: linear-gradient(135deg, #5d6d7e 0%, #4a5568 100%);
  color: white;
}

.btn-info:hover {
  background: linear-gradient(135deg, #4a5568 0%, #3d4752 100%);
  box-shadow: 0 4px 12px rgba(93, 109, 126, 0.3);
}

.btn-warning {
  background: linear-gradient(135deg, #d4af37 0%, #b8941f 100%);
  color: white;
}

.btn-warning:hover {
  background: linear-gradient(135deg, #b8941f 0%, #9c7d1a 100%);
  box-shadow: 0 4px 12px rgba(212, 175, 55, 0.3);
}

/* Inputs mejorados */
.form-control {
  border-radius: 8px;
  border: 2px solid var(--border-color);
  padding: 10px 15px;
  transition: all 0.3s ease;
  font-size: 1.05rem;
}

.form-control:focus {
  border-color: var(--primary-color);
  box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1);
}

.form-group label {
  font-weight: 500;
  font-size: 1.05rem;
  color: var(--text-primary);
  margin-bottom: 8px;
}

/* DataTables mejorados */
.dataTables_wrapper {
  padding: 10px;
}

table.dataTable {
  border-radius: 8px;
  overflow: hidden;
}

table.dataTable thead {
  background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
}

table.dataTable thead th {
  border-bottom: 2px solid var(--primary-color);
  color: var(--text-primary);
  font-weight: 600;
  padding: 12px;
}

table.dataTable tbody tr:hover {
  background-color: rgba(44, 62, 80, 0.05);
}

/* Notificaciones mejoradas */
.shiny-notification {
  border-radius: 12px;
  box-shadow: var(--shadow-lg);
  border-left: 4px solid var(--primary-color);
  font-weight: 500;
}

.shiny-notification-message {
  color: var(--primary-color);
}

/* Progress bars */
.progress {
  height: 8px;
  border-radius: 4px;
  background-color: var(--bg-light);
  overflow: hidden;
}

.progress-bar {
  background: linear-gradient(90deg, #2c3e50 0%, #34495e 100%);
}

/* Iconos mejorados */
.fa, .fas, .far {
  margin-right: 8px;
}

/* Separadores */
hr {
  border: none;
  border-top: 2px solid var(--border-color);
  margin: 20px 0;
}

/* Stats boxes */
.info-box {
  border-radius: 12px;
  box-shadow: var(--shadow-md);
  border: none;
  overflow: hidden;
}

.info-box-icon {
  border-radius: 12px 0 0 12px;
}

/* Responsive */
@media (max-width: 768px) {
  .variable-btn {
    font-size: 0.85rem;
    padding: 8px 12px;
  }

  .box-body {
    padding: 15px;
  }
}

/* Animaciones */
@keyframes fadeIn {
  from {
    opacity: 0;
    transform: translateY(10px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.box {
  animation: fadeIn 0.5s ease;
}

/* Badge para contador de variables */
.badge-counter {
  background: linear-gradient(135deg, #5d6d7e 0%, #4a5568 100%);
  color: white;
  padding: 4px 12px;
  border-radius: 20px;
  font-weight: 600;
  font-size: 0.85rem;
  box-shadow: var(--shadow-sm);
}

/* File input mejorado */
.btn-file {
  border-radius: 8px;
  background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%);
  color: #ffffff !important;
}

.btn-file:hover {
  background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%);
  color: #ffffff !important;
}

/* Label del file input */
.btn-file > input[type=\"file\"] {
  color: #ffffff !important;
}

/* Texto del botón de selección de archivo */
label.btn.btn-default.btn-file {
  color: #ffffff !important;
  font-weight: 500;
}

label.btn.btn-default.btn-file:hover {
  color: #ffffff !important;
}

/* Input file - Forzar color blanco en todos los estados */
.form-control-file + .btn-file,
.input-group .btn-file,
div[class*='shiny-input-container'] .btn-file {
  color: #ffffff !important;
}

.form-control-file + .btn-file span,
.input-group .btn-file span,
div[class*='shiny-input-container'] .btn-file span {
  color: #ffffff !important;
}

/* Placeholder del archivo */
.form-control-file + .form-control::-webkit-input-placeholder {
  color: #7f8c8d !important;
}

/* Plots mejorados */
.plot-container {
  border-radius: 12px;
  overflow: hidden;
  box-shadow: var(--shadow-md);
  background: white;
  padding: 10px;
}

/* Download cards hover effect */
.download-card:hover {
  transform: translateY(-5px);
  box-shadow: 0 10px 25px rgba(0, 0, 0, 0.15) !important;
  cursor: pointer;
}

/* Alerts mejorados */
.alert {
  border-radius: 12px;
  padding: 15px 20px;
  font-weight: 500;
}

.alert-info {
  background: linear-gradient(135deg, #dbeafe 0%, #bfdbfe 100%);
  border-left: 4px solid #2563eb;
}

.alert-warning {
  background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%);
  border-left: 4px solid #f59e0b;
}

.alert-success {
  background: linear-gradient(135deg, #d1fae5 0%, #a7f3d0 100%);
  border-left: 4px solid #10b981;
}

/* Mejora de selectores y inputs */
select.form-control {
  cursor: pointer;
}

/* Scrollbar personalizado */
::-webkit-scrollbar {
  width: 10px;
  height: 10px;
}

::-webkit-scrollbar-track {
  background: #f1f5f9;
  border-radius: 5px;
}

::-webkit-scrollbar-thumb {
  background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%);
  border-radius: 5px;
}

::-webkit-scrollbar-thumb:hover {
  background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%);
}

/* Loader animado */
.shiny-busy-panel {
  background: rgba(30, 41, 59, 0.8);
  backdrop-filter: blur(5px);
}

/* Tooltips mejorados */
.tooltip-inner {
  background: #1e293b;
  border-radius: 8px;
  padding: 8px 12px;
  font-weight: 500;
}

/* Estilos específicos para Shiny fileInput */
.shiny-input-container:not(.shiny-input-container-inline) label {
  color: var(--text-primary);
}

/* Botón de selección de archivo - FORZAR BLANCO */
.btn.btn-default.btn-file,
.btn-default.btn-file {
  background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%) !important;
  color: #ffffff !important;
  border-color: #2c3e50 !important;
}

.btn.btn-default.btn-file:hover,
.btn-default.btn-file:hover {
  background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%) !important;
  color: #ffffff !important;
  border-color: #1a252f !important;
}

.btn.btn-default.btn-file:active,
.btn.btn-default.btn-file:focus,
.btn-default.btn-file:active,
.btn-default.btn-file:focus {
  background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%) !important;
  color: #ffffff !important;
  outline: none !important;
  box-shadow: 0 0 0 3px rgba(44, 62, 80, 0.2) !important;
}
"

# UI de Login
login_ui <- div(
  style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); display: flex; align-items: center; justify-content: center; z-index: 9999;",
  tags$head(
    tags$style(HTML("
      .login-container {
        background: white;
        padding: 40px;
        border-radius: 16px;
        box-shadow: 0 20px 60px rgba(0,0,0,0.3);
        width: 400px;
        max-width: 90%;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
      }
      .login-header {
        text-align: center;
        margin-bottom: 30px;
      }
      .login-header h1 {
        color: #2c3e50;
        font-size: 2.5rem;
        margin-bottom: 10px;
        font-weight: 700;
      }
      .login-header p {
        color: #7f8c8d;
        font-size: 1.1rem;
      }
      .login-container .form-group label {
        color: #2c3e50;
        font-weight: 600;
        font-size: 1.1rem;
        margin-bottom: 8px;
      }
      .login-container .form-control {
        border: 2px solid #e0e0e0;
        border-radius: 8px;
        padding: 12px;
        font-size: 1.1rem;
        transition: all 0.3s;
      }
      .login-container .form-control:focus {
        border-color: #667eea;
        box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
        outline: none;
      }
      .btn-login {
        width: 100%;
        padding: 14px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        border-radius: 8px;
        font-size: 1.2rem;
        font-weight: 600;
        cursor: pointer;
        transition: transform 0.2s, box-shadow 0.2s;
        margin-top: 20px;
      }
      .btn-login:hover {
        transform: translateY(-2px);
        box-shadow: 0 10px 25px rgba(102, 126, 234, 0.4);
      }
      .error-message {
        background: #fee;
        color: #c33;
        padding: 12px;
        border-radius: 8px;
        margin-top: 15px;
        text-align: center;
        font-size: 1.05rem;
        display: none;
      }
      .error-message.show {
        display: block;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        $(document).on('keypress', '#login_password', function(e) {
          if(e.which == 13) {
            $('#login_btn').click();
          }
        });
      });
    "))
  ),
  div(class = "login-container",
      div(class = "login-header",
          h1(icon("chart-line"), " ThesiStats"),
          p("Ingresa la contraseña para acceder")
      ),
      passwordInput("login_password",
                    label = "Contraseña:",
                    placeholder = "Introduce tu contraseña",
                    width = "100%"),
      actionButton("login_btn", "Iniciar Sesión", class = "btn-login"),
      uiOutput("login_error")
  )
)

# UI Principal (Dashboard)
dashboard_ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      icon("chart-line", class = "fa-lg", style = "margin-right: 10px; font-size: 1.6rem;"),
      tags$span("ThesiStats", style = "font-size: 2rem; font-weight: 700; letter-spacing: -0.5px;")
    ),
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar_menu",
      selected = "welcome",  # Panel de bienvenida por defecto
      tags$div(
        style = "padding: 20px; text-align: center; border-bottom: 1px solid rgba(255,255,255,0.1);",
        tags$h4(
          "Análisis Estadístico",
          style = "color: #ecf0f1; font-weight: 600; margin: 0; font-size: 1.2rem;"
        ),
        tags$p(
          paste("Versión", APP_VERSION),
          style = "color: #bdc3c7; font-size: 1.1rem; margin: 5px 0 0 0;"
        )
      ),
      menuItem("🏠 Inicio / Bienvenida", tabName = "welcome", icon = icon("home")),
      tags$li(class = "header", "DATOS",
              style = "color: #95a5a6; font-size: 0.95rem; font-weight: 600; padding: 10px 15px; margin-top: 10px;"),
      menuItem("📁 Cargar Datos", tabName = "upload", icon = icon("database")),
      menuItem("📝 Archivo Texto", tabName = "upload_text", icon = icon("file-alt")),
      menuItem("🧩 Variables Compuestas", tabName = "composite", icon = icon("layer-group")),
      menuItem("🔍 Visualización de Data", tabName = "data_view", icon = icon("table")),
      tags$li(class = "header", "ANÁLISIS DESCRIPTIVO",
              style = "color: #95a5a6; font-size: 0.95rem; font-weight: 600; padding: 10px 15px; margin-top: 10px;"),
      menuItem("👥 Participantes", tabName = "description", icon = icon("users")),
      menuItem("📊 Potencia Estadística", tabName = "power", icon = icon("calculator")),
      tags$li(class = "header", "ANÁLISIS INFERENCIAL",
              style = "color: #95a5a6; font-size: 0.95rem; font-weight: 600; padding: 10px 15px; margin-top: 10px;"),
      menuItem("📈 Normalidad", tabName = "normality", icon = icon("chart-line")),
      menuItem("📉 Gráficos", tabName = "plots", icon = icon("chart-bar")),
      menuItem("📋 Tablas", tabName = "tables", icon = icon("table")),
      tags$li(class = "header", "EXPORTAR",
              style = "color: #95a5a6; font-size: 0.95rem; font-weight: 600; padding: 10px 15px; margin-top: 10px;"),
      menuItem("💾 Descargas", tabName = "downloads", icon = icon("download")),
      menuItem("🤖 Reporte con IA", tabName = "ai_report", icon = icon("robot")),
      tags$li(class = "header", "INFORMACIÓN",
              style = "color: #95a5a6; font-size: 0.95rem; font-weight: 600; padding: 10px 15px; margin-top: 10px;"),
      menuItem("📚 Cómo Citar", tabName = "citation", icon = icon("quote-right"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(css))),
    tabItems(
      # Tab 0: Bienvenida / Inicio
      tabItem(tabName = "welcome",
              tags$div(
                style = "padding: 30px; max-width: 1200px; margin: 0 auto;",
                
                # Encabezado principal
                tags$div(
                  style = "text-align: center; margin-bottom: 40px; padding: 40px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.2);",
                  tags$h1(
                    icon("rocket", class = "fa-2x"),
                    "Bienvenido a ThesiStats",
                    style = "color: white; font-weight: 800; margin: 20px 0; font-size: 3rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3);"
                  ),
                  tags$p(
                    "Tu herramienta completa para análisis estadístico académico",
                    style = "color: rgba(255,255,255,0.95); font-size: 1.6rem; margin: 0;"
                  )
                ),
                
                # Archivos de ejemplo
                fluidRow(
                  box(
                    title = tags$span(icon("download"), " Archivos de Ejemplo"),
                    status = "warning",
                    solidHeader = TRUE,
                    width = 12,
                    tags$p(
                      "Descarga estos archivos de ejemplo para probar la aplicación:",
                      style = "font-size: 1.4rem; margin-bottom: 20px; color: #2c3e50;"
                    ),
                    fluidRow(
                      column(6,
                             tags$div(
                               style = "text-align: center; padding: 20px; background: #f8f9fa; border-radius: 10px; margin-bottom: 15px;",
                               icon("file-excel", class = "fa-3x", style = "color: #10b981; margin-bottom: 15px;"),
                               tags$h4("Datos de Ejemplo (Excel)", style = "color: #1e293b; margin: 10px 0;"),
                               tags$p("Archivo Excel con datos de muestra", style = "color: #64748b; font-size: 1.2rem;"),
                               downloadButton("download_sample_data",
                                              label = tags$span(icon("download"), " Descargar Excel"),
                                              class = "btn-success btn-lg",
                                              style = "margin-top: 10px; font-size: 1.3rem; padding: 10px 25px;")
                             )
                      ),
                      column(6,
                             tags$div(
                               style = "text-align: center; padding: 20px; background: #f8f9fa; border-radius: 10px; margin-bottom: 15px;",
                               icon("file-alt", class = "fa-3x", style = "color: #3b82f6; margin-bottom: 15px;"),
                               tags$h4("Archivo de Texto de Ejemplo", style = "color: #1e293b; margin: 10px 0;"),
                               tags$p("Archivo TXT para análisis de texto", style = "color: #64748b; font-size: 1.2rem;"),
                               downloadButton("download_sample_text",
                                              label = tags$span(icon("download"), " Descargar TXT"),
                                              class = "btn-info btn-lg",
                                              style = "margin-top: 10px; font-size: 1.3rem; padding: 10px 25px;")
                             )
                      )
                    )
                  )
                ),
                
                # Guía paso a paso
                fluidRow(
                  box(
                    title = tags$span(icon("book-open"), " Guía de Uso Paso a Paso"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    
                    tags$div(
                      style = "padding: 20px;",
                      
                      # Paso 1
                      tags$div(
                        style = "margin-bottom: 30px; padding: 25px; background: linear-gradient(to right, #e3f2fd, #ffffff); border-left: 5px solid #2196f3; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
                        tags$h3(
                          tags$span(class = "badge", style = "background: #2196f3; font-size: 1.8rem; padding: 8px 15px; margin-right: 15px;", "1"),
                          icon("database"),
                          " Carga tus Datos",
                          style = "color: #1565c0; margin-bottom: 15px; font-size: 2rem;"
                        ),
                        tags$ul(
                          style = "font-size: 1.3rem; color: #424242; line-height: 1.8;",
                          tags$li("Ve a la pestaña ", tags$strong("📁 Cargar Datos"), " o ", tags$strong("📝 Archivo Texto")),
                          tags$li("Selecciona tu archivo Excel (.xlsx) o CSV (.csv) / Archivo de texto (.txt)"),
                          tags$li("Verifica que los datos se carguen correctamente en la vista previa"),
                          tags$li(tags$strong("Tip:"), " Asegúrate de que la primera fila contenga los nombres de las variables")
                        )
                      ),
                      
                      # Paso 2
                      tags$div(
                        style = "margin-bottom: 30px; padding: 25px; background: linear-gradient(to right, #f3e5f5, #ffffff); border-left: 5px solid #9c27b0; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
                        tags$h3(
                          tags$span(class = "badge", style = "background: #9c27b0; font-size: 1.8rem; padding: 8px 15px; margin-right: 15px;", "2"),
                          icon("layer-group"),
                          " Crea Variables Compuestas (Opcional)",
                          style = "color: #6a1b9a; margin-bottom: 15px; font-size: 2rem;"
                        ),
                        tags$ul(
                          style = "font-size: 1.3rem; color: #424242; line-height: 1.8;",
                          tags$li("Si necesitas calcular promedios o sumas de varias variables"),
                          tags$li("Ve a ", tags$strong("🧩 Variables Compuestas")),
                          tags$li("Selecciona las variables que deseas combinar"),
                          tags$li("Elige la operación (promedio o suma) y crea la nueva variable")
                        )
                      ),
                      
                      # Paso 3
                      tags$div(
                        style = "margin-bottom: 30px; padding: 25px; background: linear-gradient(to right, #fff3e0, #ffffff); border-left: 5px solid #ff9800; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
                        tags$h3(
                          tags$span(class = "badge", style = "background: #ff9800; font-size: 1.8rem; padding: 8px 15px; margin-right: 15px;", "3"),
                          icon("chart-line"),
                          " Realiza Análisis Descriptivos",
                          style = "color: #e65100; margin-bottom: 15px; font-size: 2rem;"
                        ),
                        tags$ul(
                          style = "font-size: 1.3rem; color: #424242; line-height: 1.8;",
                          tags$li(tags$strong("👥 Participantes:"), " Analiza datos demográficos y descriptivos"),
                          tags$li(tags$strong("📊 Potencia Estadística:"), " Calcula el tamaño de muestra necesario"),
                          tags$li(tags$strong("🔍 Visualización de Data:"), " Explora tus datos en formato tabla")
                        )
                      ),
                      
                      # Paso 4
                      tags$div(
                        style = "margin-bottom: 30px; padding: 25px; background: linear-gradient(to right, #e8f5e9, #ffffff); border-left: 5px solid #4caf50; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
                        tags$h3(
                          tags$span(class = "badge", style = "background: #4caf50; font-size: 1.8rem; padding: 8px 15px; margin-right: 15px;", "4"),
                          icon("chart-bar"),
                          " Análisis Inferenciales",
                          style = "color: #2e7d32; margin-bottom: 15px; font-size: 2rem;"
                        ),
                        tags$ul(
                          style = "font-size: 1.3rem; color: #424242; line-height: 1.8;",
                          tags$li(tags$strong("📈 Normalidad:"), " Verifica supuestos de normalidad univariada y multivariada"),
                          tags$li(tags$strong("📉 Gráficos:"), " Genera visualizaciones profesionales"),
                          tags$li(tags$strong("📋 Tablas:"), " Crea tablas estadísticas completas")
                        )
                      ),
                      
                      # Paso 5
                      tags$div(
                        style = "margin-bottom: 30px; padding: 25px; background: linear-gradient(to right, #fce4ec, #ffffff); border-left: 5px solid #e91e63; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
                        tags$h3(
                          tags$span(class = "badge", style = "background: #e91e63; font-size: 1.8rem; padding: 8px 15px; margin-right: 15px;", "5"),
                          icon("download"),
                          " Exporta tus Resultados",
                          style = "color: #c2185b; margin-bottom: 15px; font-size: 2rem;"
                        ),
                        tags$ul(
                          style = "font-size: 1.3rem; color: #424242; line-height: 1.8;",
                          tags$li(tags$strong("💾 Descargas:"), " Descarga tus datos procesados en formato Excel"),
                          tags$li(tags$strong("🤖 Reporte con IA:"), " Genera un reporte automático con interpretación de resultados"),
                          tags$li("Todos los gráficos y tablas pueden descargarse individualmente")
                        )
                      ),
                      
                      # Paso 6
                      tags$div(
                        style = "padding: 25px; background: linear-gradient(to right, #e0f2f1, #ffffff); border-left: 5px solid #00897b; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
                        tags$h3(
                          tags$span(class = "badge", style = "background: #00897b; font-size: 1.8rem; padding: 8px 15px; margin-right: 15px;", "6"),
                          icon("quote-right"),
                          " Cita la Aplicación",
                          style = "color: #00695c; margin-bottom: 15px; font-size: 2rem;"
                        ),
                        tags$ul(
                          style = "font-size: 1.3rem; color: #424242; line-height: 1.8;",
                          tags$li("No olvides citar ThesiStats en tus trabajos académicos"),
                          tags$li("Consulta ", tags$strong("📚 Cómo Citar"), " para ver la referencia en formato APA")
                        )
                      )
                    )
                  )
                ),
                
                # Consejos adicionales
                fluidRow(
                  box(
                    title = tags$span(icon("lightbulb"), " Consejos y Mejores Prácticas"),
                    status = "success",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tags$div(
                      style = "padding: 15px;",
                      tags$ul(
                        style = "font-size: 1.3rem; color: #2c3e50; line-height: 1.8;",
                        tags$li(icon("check-circle", style = "color: #27ae60;"), " ", tags$strong("Prepara tus datos:"), " Asegúrate de que no haya valores faltantes críticos"),
                        tags$li(icon("check-circle", style = "color: #27ae60;"), " ", tags$strong("Nombres claros:"), " Usa nombres descriptivos para tus variables"),
                        tags$li(icon("check-circle", style = "color: #27ae60;"), " ", tags$strong("Guarda tu progreso:"), " Descarga tus datos procesados regularmente"),
                        tags$li(icon("check-circle", style = "color: #27ae60;"), " ", tags$strong("Verifica normalidad:"), " Antes de análisis paramétricos, revisa los supuestos"),
                        tags$li(icon("check-circle", style = "color: #27ae60;"), " ", tags$strong("Usa el reporte IA:"), " Para obtener interpretaciones automáticas de tus resultados")
                      )
                    )
                  )
                ),
                
                # Botón para comenzar
                tags$div(
                  style = "text-align: center; margin-top: 40px; padding: 30px; background: #f8f9fa; border-radius: 15px;",
                  tags$h3("¿Listo para comenzar?", style = "color: #2c3e50; margin-bottom: 20px; font-size: 2.2rem;"),
                  tags$p("Haz clic en el menú lateral para acceder a las diferentes funcionalidades",
                         style = "color: #7f8c8d; font-size: 1.4rem; margin-bottom: 25px;"),
                  actionButton("goto_upload",
                               label = tags$span(icon("arrow-right"), " Ir a Cargar Datos"),
                               class = "btn-primary btn-lg",
                               style = "font-size: 1.6rem; padding: 15px 40px; border-radius: 50px; box-shadow: 0 4px 15px rgba(0,0,0,0.2);")
                )
              )
      ),
      
      # Tab 1: Cargar datos
      tabItem(tabName = "upload",
              tags$div(
                style = "padding: 20px 0;",
                tags$h2(
                  icon("database"),
                  "Cargar Datos",
                  style = "color: #1e293b; font-weight: 700; margin-bottom: 10px;"
                ),
                tags$p(
                  "Sube tu archivo de datos en formato Excel (.xlsx) o CSV (.csv) para comenzar el análisis.",
                  style = "color: #64748b; font-size: 1.4rem; margin-bottom: 30px;"
                )
              ),
              fluidRow(
                box(
                  title = tags$span(icon("cloud-upload-alt"), " Cargar Archivo de Datos"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    style = "padding: 20px; text-align: center; background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%); border-radius: 12px; border: 2px dashed #cbd5e1; margin-bottom: 20px;",
                    icon("file-excel", class = "fa-3x", style = "color: #10b981; margin-bottom: 15px;"),
                    fileInput("file", NULL,
                              accept = c(".xlsx", ".csv"),
                              buttonLabel = tags$span(icon("folder-open"), " Seleccionar archivo"),
                              placeholder = "Ningún archivo seleccionado")
                  ),
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    tags$div(
                      style = "margin-top: 20px;",
                      tags$h4(
                        icon("table"),
                        "Vista Previa de los Datos",
                        style = "color: #1e293b; font-weight: 600; margin-bottom: 15px;"
                      ),
                      DT::dataTableOutput("data_preview")
                    )
                  )
                )
              )
      ),
      
      # Tab 1.5: Cargar archivo de texto
      tabItem(tabName = "upload_text",
              tags$div(
                style = "padding: 20px 0;",
                tags$h2(
                  icon("file-alt"),
                  "Archivo de Texto",
                  style = "color: #1e293b; font-weight: 700; margin-bottom: 10px;"
                ),
                tags$p(
                  "Carga el archivo de texto con la estructura de variables compuestas para calcular la fiabilidad.",
                  style = "color: #64748b; font-size: 1.4rem; margin-bottom: 30px;"
                )
              ),
              fluidRow(
                box(
                  title = tags$span(icon("file-code"), " Estructura de Variables"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    class = "alert alert-info",
                    style = "background: linear-gradient(135deg, #dbeafe 0%, #bfdbfe 100%); border: none; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                    icon("info-circle", style = "color: #2563eb;"),
                    tags$strong(" Importante:", style = "color: #1e40af;"),
                    tags$span(" Este archivo debe contener la estructura de variables compuestas para calcular fiabilidad (omega, alfa, etc.).", style = "color: #1e40af;")
                  ),
                  tags$div(
                    style = "padding: 20px; text-align: center; background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%); border-radius: 12px; border: 2px dashed #cbd5e1; margin-bottom: 20px;",
                    icon("file-alt", class = "fa-3x", style = "color: #06b6d4; margin-bottom: 15px;"),
                    fileInput("text_file", NULL,
                              accept = c(".txt"),
                              buttonLabel = tags$span(icon("folder-open"), " Seleccionar archivo .txt"),
                              placeholder = "Ningún archivo seleccionado")
                  ),
                  conditionalPanel(
                    condition = "output.text_file_uploaded",
                    tags$div(
                      style = "margin-top: 20px;",
                      tags$h4(
                        icon("eye"),
                        "Vista Previa del Contenido",
                        style = "color: #1e293b; font-weight: 600; margin-bottom: 15px;"
                      ),
                      tags$div(
                        style = "background: #1e293b; color: #10b981; padding: 15px; border-radius: 8px; font-family: 'Courier New', monospace; font-size: 0.9rem;",
                        verbatimTextOutput("text_preview")
                      )
                    )
                  )
                )
              )
      ),
      
      # Tab 1.75: Variables compuestas
      tabItem(tabName = "composite",
              tags$div(
                style = "padding: 20px 0;",
                tags$h2(
                  icon("layer-group"),
                  "Variables Compuestas",
                  style = "color: #1e293b; font-weight: 700; margin-bottom: 10px;"
                ),
                tags$p(
                  "Genera variables compuestas a partir de la estructura definida en el archivo de texto.",
                  style = "color: #64748b; font-size: 1.4rem; margin-bottom: 30px;"
                )
              ),
              fluidRow(
                box(
                  title = tags$span(icon("magic"), " Generador de Variables"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    class = "alert alert-warning",
                    style = "background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%); border: none; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                    icon("exclamation-triangle", style = "color: #d97706;"),
                    tags$strong(" Requisito:", style = "color: #92400e;"),
                    tags$span(" Asegúrate de haber cargado primero el archivo de datos y el archivo de texto.", style = "color: #000000;")
                  ),
                  tags$div(
                    class = "alert alert-info",
                    style = "background: linear-gradient(135deg, #dbeafe 0%, #bfdbfe 100%); border: none; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                    icon("info-circle", style = "color: #2563eb;"),
                    tags$strong(" Información:", style = "color: #1e40af;"),
                    tags$span(" Las variables compuestas se agregarán automáticamente a tu base de datos y estarán disponibles para todos los análisis.", style = "color: #1e40af;")
                  ),
                  tags$div(
                    style = "text-align: center; padding: 20px;",
                    actionButton("generate_composite",
                                 tags$span(icon("cogs"), " Generar y Agregar Variables Compuestas"),
                                 class = "btn-primary btn-lg",
                                 style = "font-size: 1.1rem; padding: 15px 40px;")
                  ),
                  conditionalPanel(
                    condition = "output.composite_generated",
                    tags$hr(style = "margin: 30px 0;"),
                    tags$div(
                      class = "alert alert-success",
                      style = "background: linear-gradient(135deg, #d1fae5 0%, #a7f3d0 100%); border: none; border-radius: 8px; padding: 20px; margin-bottom: 20px;",
                      icon("check-circle", class = "fa-3x", style = "color: #059669; margin-bottom: 15px;"),
                      tags$h3("¡Variables Agregadas Exitosamente!",
                              style = "color: #065f46; font-weight: 600; margin: 10px 0 15px 0;"),
                      tags$p("Las nuevas variables compuestas ya están disponibles en tu base de datos y pueden ser utilizadas en todos los análisis (Normalidad, Gráficos, Tablas, etc.).",
                             style = "color: #047857; font-size: 1.35rem; margin-bottom: 10px;"),
                      tags$p(
                        icon("arrow-right", style = "color: #059669;"),
                        tags$strong(" Ve a la pestaña '🔍 Visualización de Data' para ver tu base completa con las nuevas variables."),
                        style = "color: #065f46; font-size: 1.35rem; margin-bottom: 0;"
                      )
                    ),
                    tags$div(
                      style = "background: #1e293b; color: #10b981; padding: 20px; border-radius: 8px; font-family: 'Courier New', monospace; margin-bottom: 20px;",
                      verbatimTextOutput("composite_summary")
                    ),
                    tags$div(
                      style = "text-align: center;",
                      tags$p(
                        icon("lightbulb", style = "color: #d4af37;"),
                        tags$strong(" Opcional:"),
                        " Si deseas guardar una copia del dataset completo, puedes descargarlo aquí:",
                        style = "color: #64748b; margin-bottom: 15px; font-size: 1.35rem;"
                      ),
                      downloadButton("download_composite",
                                     tags$span(icon("file-download"), " Descargar Copia del Dataset Completo (Excel)"),
                                     class = "btn-info",
                                     style = "font-size: 1rem; padding: 12px 30px;")
                    )
                  )
                )
              )
      ),
      
      # Tab 1.8: Visualización de Data
      tabItem(tabName = "data_view",
              tags$div(
                style = "padding: 20px 0;",
                tags$h2(
                  icon("table"),
                  "Visualización de Data",
                  style = "color: #1e293b; font-weight: 700; margin-bottom: 10px;"
                ),
                tags$p(
                  "Aquí puedes ver tu base de datos completa, incluyendo las variables compuestas que hayas agregado.",
                  style = "color: #64748b; font-size: 1.4rem; margin-bottom: 30px;"
                )
              ),
              fluidRow(
                box(
                  title = tags$span(icon("database"), " Base de Datos Completa"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  conditionalPanel(
                    condition = "output.data_available",
                    tags$div(
                      class = "alert alert-info",
                      style = "background: linear-gradient(135deg, #dbeafe 0%, #bfdbfe 100%); border: none; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
                      icon("info-circle", style = "color: #2563eb;"),
                      tags$span(
                        tags$strong(" Total de registros: ", style = "color: #1e40af;"),
                        textOutput("total_rows", inline = TRUE),
                        tags$strong(" | Total de variables: ", style = "color: #1e40af;"),
                        textOutput("total_cols", inline = TRUE),
                        style = "color: #1e40af;"
                      )
                    ),
                    DT::dataTableOutput("full_data_table"),
                    tags$hr(style = "margin: 20px 0;"),
                    tags$div(
                      style = "text-align: center;",
                      downloadButton("download_full_data",
                                     tags$span(icon("file-download"), " Descargar Base Completa (Excel)"),
                                     class = "btn-success",
                                     style = "font-size: 1rem; padding: 12px 30px;")
                    )
                  ),
                  conditionalPanel(
                    condition = "!output.data_available",
                    tags$div(
                      style = "text-align: center; padding: 40px;",
                      icon("exclamation-circle", class = "fa-3x", style = "color: #94a3b8; margin-bottom: 15px;"),
                      tags$p("No hay datos cargados. Por favor, carga un archivo de datos primero.",
                             style = "color: #64748b; font-size: 1.4rem;")
                    )
                  )
                )
              )
      ),
      
      # Tab 2: Descripción de participantes
      tabItem(tabName = "description",
              fluidRow(
                box(
                  title = "Selección de Variables Categóricas", status = "primary", solidHeader = TRUE, width = 12,
                  div(class = "section-title", "Seleccionar variables categóricas:"),
                  div(class = "variable-container",
                      uiOutput("variable_buttons_categorical")
                  ),
                  br(),
                  fluidRow(
                    column(6,
                           actionButton("select_all_categorical", "Seleccionar Todas", class = "btn-info")
                    ),
                    column(6,
                           actionButton("clear_all_categorical", "Limpiar Selección", class = "btn-warning")
                    )
                  ),
                  hr(),
                  div(
                    strong("Variables seleccionadas: "),
                    textOutput("selected_count_categorical", inline = TRUE)
                  ),
                  br(),
                  actionButton("calculate_percentages", "Calcular Porcentajes", class = "btn-primary")
                )
              ),
              fluidRow(
                box(
                  title = "Resultados de Porcentajes", status = "success", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("percentage_results")
                )
              ),
              fluidRow(
                box(
                  title = "Estadísticas de Edad", status = "info", solidHeader = TRUE, width = 12,
                  textInput("age_variable", "Nombre de la variable de edad:",
                            value = "Edad", placeholder = "Escriba el nombre de la variable"),
                  hr(),
                  verbatimTextOutput("age_stats")
                )
              )
      ),
      
      # Tab 3: Análisis de potencia
      tabItem(tabName = "power",
              fluidRow(
                box(
                  title = "Análisis de Potencia", status = "primary", solidHeader = TRUE, width = 12,
                  tags$div(
                    style = "font-size: 1.35rem;",
                    numericInput("effect_size", "Tamaño del efecto (r):", value = 0.27, min = 0, max = 1, step = 0.01)
                  ),
                  tags$div(
                    style = "font-size: 1.35rem;",
                    numericInput("power_level", "Potencia deseada:", value = 0.80, min = 0, max = 1, step = 0.01)
                  ),
                  tags$div(
                    style = "font-size: 1.35rem;",
                    numericInput("alpha_level", "Nivel de significancia:", value = 0.05, min = 0, max = 1, step = 0.01)
                  ),
                  actionButton("calculate_power", "Calcular Potencia", class = "btn-primary"),
                  hr(),
                  verbatimTextOutput("power_results")
                )
              )
      ),
      
      # Tab 4: Normalidad
      tabItem(tabName = "normality",
              fluidRow(
                box(
                  title = "Selección de Variables para Normalidad", status = "primary", solidHeader = TRUE, width = 12,
                  div(class = "section-title", "Seleccionar variables:"),
                  div(class = "variable-container",
                      uiOutput("variable_buttons_normality")
                  ),
                  br(),
                  fluidRow(
                    column(6,
                           actionButton("select_all_normality", "Seleccionar Todas", class = "btn-info")
                    ),
                    column(6,
                           actionButton("clear_all_normality", "Limpiar Selección", class = "btn-warning")
                    )
                  ),
                  hr(),
                  div(
                    strong("Variables seleccionadas: "),
                    textOutput("selected_count_normality", inline = TRUE)
                  ),
                  br(),
                  actionButton("test_normality", "Realizar Pruebas", class = "btn-primary")
                )
              ),
              fluidRow(
                box(
                  title = "Resultados de Normalidad", status = "success", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("normality_results"),
                  br(),
                  downloadButton("download_normality_table", "Descargar Normalidad (Excel)", class = "btn-success")
                )
              ),
              
              # Normalidad Multivariada
              fluidRow(
                box(
                  title = "Normalidad Multivariada",
                  status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3, numericInput("mv_xmin", "X mínimo:", value = 15, step = 0.1)),
                    column(3, numericInput("mv_xmax", "X máximo:", value = 18, step = 0.1)),
                    column(3, numericInput("mv_ymin", "Y mínimo:", value = 3, step = 0.1)),
                    column(3, numericInput("mv_ymax", "Y máximo:", value = 5, step = 0.1))
                  ),
                  hr(),
                  plotOutput("multivariate_plot", height = "500px"),
                  br(),
                  downloadButton("download_multivariate_plot_btn",
                                 "Descargar Gráfico Multivariada (JPG)",
                                 class = "btn-success")
                )
              )
              
      ),
      
      # Tab 5: Gráficos
      tabItem(tabName = "plots",
              fluidRow(
                box(
                  title = "Selección de Variables para Gráficos", status = "primary", solidHeader = TRUE, width = 12,
                  div(class = "section-title", "Seleccionar variables:"),
                  div(class = "variable-container",
                      uiOutput("variable_buttons_plots")
                  ),
                  br(),
                  fluidRow(
                    column(6,
                           actionButton("select_all_plots", "Seleccionar Todas", class = "btn-info")
                    ),
                    column(6,
                           actionButton("clear_all_plots", "Limpiar Selección", class = "btn-warning")
                    )
                  ),
                  hr(),
                  div(
                    strong("Variables seleccionadas: "),
                    textOutput("selected_count_plots", inline = TRUE)
                  )
                )
              ),
              fluidRow(
                box(
                  title = tags$span(icon("chart-area"), " Diagrama de Cajas (Boxplots)"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    class = "plot-container",
                    plotOutput("boxplots", height = "500px")
                  ),
                  tags$hr(style = "margin: 20px 0;"),
                  tags$div(
                    style = "text-align: center;",
                    downloadButton("download_boxplot_btn",
                                   tags$span(icon("download"), " Descargar Boxplot (JPG - 600 DPI)"),
                                   class = "btn-success",
                                   style = "padding: 12px 30px;")
                  )
                )
              )
      ),
      
      # Tab 6: Tablas de resultados
      tabItem(tabName = "tables",
              fluidRow(
                box(
                  title = "Selección de Variables para Análisis", status = "primary", solidHeader = TRUE, width = 12,
                  div(class = "section-title", "Seleccionar variables:"),
                  div(class = "variable-container",
                      uiOutput("variable_buttons_tables")
                  ),
                  br(),
                  fluidRow(
                    column(6,
                           actionButton("select_all_tables", "Seleccionar Todas", class = "btn-info")
                    ),
                    column(6,
                           actionButton("clear_all_tables", "Limpiar Selección", class = "btn-warning")
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(6,
                           div(
                             strong("Variables seleccionadas: "),
                             textOutput("selected_count_tables", inline = TRUE)
                           )
                    ),
                    column(6,
                           uiOutput("group_variable")
                    )
                  ),
                  tags$div(
                    class = "alert alert-warning",
                    style = "background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%); border: none; border-radius: 8px; padding: 12px; margin: 15px 0;",
                    icon("info-circle", style = "color: #d97706;"),
                    tags$span(" Nota: Para comparaciones por grupo, la variable de agrupación debe tener exactamente 2 niveles (ej: Mujer/Varón).",
                              style = "color: #92400e; font-size: 1.3rem;")
                  ),
                  br(),
                  fluidRow(
                    column(12,
                           actionButton("calculate_tables", "Calcular Tablas", class = "btn-primary")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Tabla 1: Descriptivos + Fiabilidad", status = "primary", solidHeader = TRUE, width = 12,
                  p("Requiere archivo de texto cargado para calcular fiabilidad"),
                  DT::dataTableOutput("tabla1"),
                  br(),
                  downloadButton("download_tabla1", "Descargar Tabla 1 (Excel)", class = "btn-success")
                )
              ),
              fluidRow(
                box(
                  title = "Tabla 2: Matriz de Correlaciones", status = "success", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("correlation_method", "Método de correlación:",
                                       choices = c("Pearson" = "pearson",
                                                   "Spearman" = "spearman"),
                                       selected = "pearson")
                    ),
                    column(4,
                           checkboxInput("winsorize_corr", "Aplicar winsorización", TRUE)
                    ),
                    column(4,
                           checkboxInput("show_pval", "Mostrar p-valores", FALSE)
                    )
                  ),
                  hr(),
                  DT::dataTableOutput("correlation_table"),
                  br(),
                  downloadButton("download_correlation_matrix", "Descargar Tabla 2 (Excel)", class = "btn-success")
                )
              ),
              fluidRow(
                box(
                  title = "Tabla 3: Comparaciones por Grupo", status = "info", solidHeader = TRUE, width = 12,
                  checkboxInput("robust_analysis",
                                tags$span(icon("shield-alt"), " d cohen robusta"),
                                TRUE),
                  tags$hr(style = "margin: 10px 0 20px 0;"),
                  conditionalPanel(
                    condition = "output.comparative_available",
                    DT::dataTableOutput("comparative_table"),
                    br(),
                    downloadButton("download_comparative_table", "Descargar Tabla 3 (Excel)", class = "btn-success")
                  ),
                  conditionalPanel(
                    condition = "!output.comparative_available",
                    tags$div(
                      style = "text-align: center; padding: 40px; color: #64748b;",
                      icon("info-circle", class = "fa-2x", style = "margin-bottom: 10px;"),
                      tags$p("Selecciona una variable de agrupación y presiona 'Calcular Tablas' para ver las comparaciones.",
                             style = "font-size: 1.35rem;")
                    )
                  )
                )
              )
      ),
      
      # Tab 7: Descargas
      tabItem(tabName = "downloads",
              tags$div(
                style = "padding: 20px 0;",
                tags$h2(
                  icon("download"),
                  "Centro de Descargas",
                  style = "color: #1e293b; font-weight: 700; margin-bottom: 10px;"
                ),
                tags$p(
                  "Descarga todos los resultados de tus análisis en formato Excel o imagen de alta calidad.",
                  style = "color: #64748b; font-size: 1.4rem; margin-bottom: 30px;"
                )
              ),
              
              # Archivos Excel
              tags$h3(
                icon("file-excel"),
                "Archivos Excel",
                style = "color: #27ae60; font-weight: 600; margin: 30px 0 20px 0;"
              ),
              fluidRow(
                column(4,
                       tags$div(
                         class = "download-card",
                         style = "background: linear-gradient(135deg, #d5f4e6 0%, #abebc6 100%); padding: 30px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s ease; margin-bottom: 20px;",
                         icon("table", class = "fa-3x", style = "color: #27ae60; margin-bottom: 15px;"),
                         tags$h4("Normalidad", style = "color: #1e8449; font-weight: 600; margin-bottom: 10px;"),
                         tags$p("Apéndice A", style = "color: #229954; margin-bottom: 20px; font-size: 1.35rem;"),
                         downloadButton("download_normality",
                                        tags$span(icon("download"), " Descargar"),
                                        class = "btn-success",
                                        style = "width: 100%; padding: 12px;")
                       )
                ),
                column(4,
                       tags$div(
                         class = "download-card",
                         style = "background: linear-gradient(135deg, #d5dbdb 0%, #aeb6bf 100%); padding: 30px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s ease; margin-bottom: 20px;",
                         icon("chart-bar", class = "fa-3x", style = "color: #5d6d7e; margin-bottom: 15px;"),
                         tags$h4("Descriptivos", style = "color: #4a5568; font-weight: 600; margin-bottom: 10px;"),
                         tags$p("Tabla 1", style = "color: #5d6d7e; margin-bottom: 20px; font-size: 1.35rem;"),
                         downloadButton("download_descriptives",
                                        tags$span(icon("download"), " Descargar"),
                                        class = "btn-info",
                                        style = "width: 100%; padding: 12px;")
                       )
                ),
                column(4,
                       tags$div(
                         class = "download-card",
                         style = "background: linear-gradient(135deg, #fef5e7 0%, #fdebd0 100%); padding: 30px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s ease; margin-bottom: 20px;",
                         icon("project-diagram", class = "fa-3x", style = "color: #d4af37; margin-bottom: 15px;"),
                         tags$h4("Correlaciones", style = "color: #b8941f; font-weight: 600; margin-bottom: 10px;"),
                         tags$p("Tabla 2", style = "color: #d4af37; margin-bottom: 20px; font-size: 1.35rem;"),
                         downloadButton("download_correlations",
                                        tags$span(icon("download"), " Descargar"),
                                        class = "btn-warning",
                                        style = "width: 100%; padding: 12px;")
                       )
                )
              ),
              fluidRow(
                column(4,
                       tags$div(
                         class = "download-card",
                         style = "background: linear-gradient(135deg, #e8daef 0%, #d7bde2 100%); padding: 30px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s ease; margin-bottom: 20px;",
                         icon("balance-scale", class = "fa-3x", style = "color: #8e44ad; margin-bottom: 15px;"),
                         tags$h4("Comparaciones", style = "color: #6c3483; font-weight: 600; margin-bottom: 10px;"),
                         tags$p("Tabla 3", style = "color: #8e44ad; margin-bottom: 20px; font-size: 1.35rem;"),
                         downloadButton("download_comparative",
                                        tags$span(icon("download"), " Descargar"),
                                        class = "btn-primary",
                                        style = "width: 100%; padding: 12px;")
                       )
                )
              ),
              
              # Imágenes
              tags$h3(
                icon("image"),
                "Gráficos de Alta Resolución (600 DPI)",
                style = "color: #2c3e50; font-weight: 600; margin: 30px 0 20px 0;"
              ),
              fluidRow(
                column(6,
                       tags$div(
                         class = "download-card",
                         style = "background: linear-gradient(135deg, #e8eaf6 0%, #c5cae9 100%); padding: 30px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s ease; margin-bottom: 20px;",
                         icon("chart-area", class = "fa-3x", style = "color: #2c3e50; margin-bottom: 15px;"),
                         tags$h4("Boxplots", style = "color: #1a252f; font-weight: 600; margin-bottom: 10px;"),
                         tags$p("Apéndice B", style = "color: #34495e; margin-bottom: 20px; font-size: 1.35rem;"),
                         downloadButton("download_boxplot",
                                        tags$span(icon("download"), " Descargar JPG"),
                                        class = "btn-primary",
                                        style = "width: 100%; padding: 12px;")
                       )
                ),
                column(6,
                       tags$div(
                         class = "download-card",
                         style = "background: linear-gradient(135deg, #e8eaf6 0%, #c5cae9 100%); padding: 30px; border-radius: 12px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1); transition: all 0.3s ease; margin-bottom: 20px;",
                         icon("bezier-curve", class = "fa-3x", style = "color: #2c3e50; margin-bottom: 15px;"),
                         tags$h4("Normalidad Multivariada", style = "color: #1a252f; font-weight: 600; margin-bottom: 10px;"),
                         tags$p("Apéndice C", style = "color: #34495e; margin-bottom: 20px; font-size: 1.35rem;"),
                         downloadButton("download_multivariate_plot",
                                        tags$span(icon("download"), " Descargar JPG"),
                                        class = "btn-primary",
                                        style = "width: 100%; padding: 12px;")
                       )
                )
              )
      ),
      
      # Tab 8: Reporte con IA (ChatGPT)
      tabItem(tabName = "ai_report",
              # Banner principal
              div(style = "background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%); color: white; padding: 20px;
                   border-radius: 8px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.12);",
                  h3(style="margin: 0; font-weight: 700;",
                     icon("robot"), " Generar Reporte Académico con ChatGPT"),
                  p(style="margin: 10px 0 0 0; font-size: 14px;",
                    "Esta sección permite generar automáticamente secciones académicas de métodos y resultados usando ChatGPT API.")
              ),
              
              # Pasos e información
              fluidRow(
                column(12,
                       box(width = NULL, status = "success", solidHeader = TRUE,
                           title = tags$span(icon("list-ol"), " Pasos:"),
                           tags$ol(
                             style = "font-size: 1.35rem; line-height: 1.8;",
                             tags$li("Completa tus análisis en las pestañas anteriores (Participantes, Potencia, Normalidad, Tablas)"),
                             tags$li(HTML('Ingresa tu API key de OpenAI (obtenla en <a target="_blank" href="https://platform.openai.com/">OpenAI Platform</a>)')),
                             tags$li("Selecciona el idioma para tu reporte"),
                             tags$li("Haz clic en \"Generar Reporte\" y espera los resultados")
                           ),
                           tags$hr(),
                           tags$p(
                             icon("info-circle", style = "color: #27ae60;"),
                             HTML('<strong>Nota:</strong> La aplicación usa el modelo fijo <strong>GPT-4.1</strong> para generar reportes académicos profesionales.'),
                             style = "color: #475569; margin-top: 10px; font-size: 1.35rem;"
                           )
                       )
                )
              ),
              
              # Controles principales
              fluidRow(
                column(8,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(icon("key"), " API Key de OpenAI"),
                           passwordInput("ai_api_key", label = NULL,
                                         placeholder = "sk-...", width = "100%"),
                           tags$small(
                             icon("lock", style = "color: #64748b;"),
                             " Tu API key no se almacena y solo se usa durante esta sesión",
                             style = "color: #64748b; font-size: 1.2rem;"
                           )
                       )
                ),
                column(4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = tags$span(icon("language"), " Idioma del Reporte"),
                           selectInput("ai_lang", label = NULL,
                                       choices = c("English", "Español"),
                                       selected = "Español", width = "100%")
                       )
                )
              ),
              
              # Botón de generación
              fluidRow(
                column(12,
                       div(style="display: flex; gap: 15px; align-items: center; margin-bottom: 20px;",
                           actionButton("ai_generate_btn",
                                        label = tags$div(
                                          icon("wand-magic-sparkles", style = "margin-right: 8px;"),
                                          tags$strong("Generar Reporte con IA")
                                        ),
                                        class = "btn-success btn-lg",
                                        style = "padding: 14px 30px; font-size: 16px;")
                       )
                )
              ),
              
              # Preview y descarga
              fluidRow(
                column(12,
                       box(width = NULL, status = "info", solidHeader = TRUE, collapsible = TRUE,
                           title = tags$span(icon("file-alt"), " Vista Previa del Reporte"),
                           htmlOutput("ai_report_html"),
                           tags$hr(),
                           div(style="display: flex; gap: 10px; margin-top: 15px;",
                               downloadButton("ai_download_docx",
                                              label = tags$div(icon("download"), " Descargar DOCX"),
                                              class = "btn-primary")
                           )
                       )
                )
              )
      ),
      
      # Tab: Cómo Citar
      tabItem(tabName = "citation",
              tags$div(
                style = "padding: 20px 0;",
                tags$h2(
                  icon("quote-right"),
                  "Cómo Citar ThesiStats",
                  style = "color: #1e293b; font-weight: 700; margin-bottom: 10px;"
                ),
                tags$p(
                  "Si has utilizado esta aplicación en tu investigación, por favor cítala usando el formato APA:",
                  style = "color: #64748b; font-size: 1.4rem; margin-bottom: 30px;"
                )
              ),
              fluidRow(
                box(
                  title = tags$span(icon("graduation-cap"), " Formato de Citación APA 7"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    style = "padding: 25px; background: #f8fafc; border-left: 4px solid #3b82f6; border-radius: 8px; margin-bottom: 20px;",
                    tags$p(
                      style = "font-size: 1.6rem; line-height: 1.8; color: #1e293b; margin: 0; font-family: 'Times New Roman', serif;",
                      "Ventura-León, J. (2025). ", tags$em("ThesiStats app"), " (Versión 2.1.0) [Aplicación web]. ",
                      tags$a(
                        href = "https://jventural-thesistats-app.share.connect.posit.cloud/",
                        target = "_blank",
                        "https://jventural-thesistats-app.share.connect.posit.cloud/"
                      )
                    )
                  ),
                  tags$div(
                    style = "text-align: center; margin-top: 20px;",
                    tags$button(
                      class = "btn btn-success btn-lg",
                      onclick = "navigator.clipboard.writeText('Ventura-León, J. (2025). ThesiStats app (Versión 2.1.0) [Aplicación web]. https://jventural-thesistats-app.share.connect.posit.cloud/'); alert('Citación copiada al portapapeles');",
                      icon("copy"),
                      " Copiar Citación"
                    )
                  )
                ),
                box(
                  title = tags$span(icon("info-circle"), " Información Adicional"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    style = "padding: 15px;",
                    tags$ul(
                      style = "font-size: 1.35rem; line-height: 1.8; color: #475569;",
                      tags$li(tags$strong("Autor: "), "José Ventura-León"),
                      tags$li(tags$strong("Año: "), "2025"),
                      tags$li(tags$strong("Versión: "), "2.1.0"),
                      tags$li(tags$strong("Tipo: "), "Aplicación web interactiva para análisis estadístico"),
                      tags$li(tags$strong("URL: "), tags$a(
                        href = "https://jventural-thesistats-app.share.connect.posit.cloud/",
                        target = "_blank",
                        "https://jventural-thesistats-app.share.connect.posit.cloud/"
                      ))
                    )
                  )
                ),
                box(
                  title = tags$span(icon("heart", style = "color: #e74c3c;"), " Agradecimiento"),
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    style = "padding: 15px; text-align: center;",
                    tags$p(
                      style = "font-size: 1.45rem; color: #475569; line-height: 1.6;",
                      "Gracias por utilizar ThesiStats. Tu citación ayuda a dar crédito al trabajo realizado y permite que más investigadores conozcan esta herramienta."
                    )
                  )
                ),
                box(
                  title = tags$span(icon("bug", style = "color: #dc2626;"), " Reportar Errores o Bugs"),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    style = "padding: 20px; text-align: center;",
                    tags$p(
                      icon("envelope", style = "color: #dc2626; font-size: 2rem;"),
                      style = "margin-bottom: 15px;"
                    ),
                    tags$p(
                      style = "font-size: 1.4rem; color: #475569; line-height: 1.6; margin-bottom: 10px;",
                      "Si encuentras algún error o bug en la aplicación, por favor repórtalo a:"
                    ),
                    tags$p(
                      tags$a(
                        href = "mailto:info@joseventuraleon.com",
                        style = "font-size: 1.5rem; color: #dc2626; font-weight: 600; text-decoration: none;",
                        icon("envelope"),
                        " info@joseventuraleon.com"
                      )
                    ),
                    tags$p(
                      style = "font-size: 1.3rem; color: #64748b; margin-top: 15px;",
                      "Tu retroalimentación ayuda a mejorar la aplicación para toda la comunidad."
                    )
                  )
                )
              )
      )
    )
  )
)

# UI principal con sistema de autenticación
ui <- uiOutput("main_ui")

# Server
server <- function(input, output, session) {
  # Estado de autenticación
  authenticated <- reactiveVal(FALSE)
  
  # Renderizar UI según estado de autenticación
  output$main_ui <- renderUI({
    if (authenticated()) {
      tagList(
        dashboard_ui
      )
    } else {
      login_ui
    }
  })
  
  # Manejar intento de login
  observeEvent(input$login_btn, {
    if (!is.null(input$login_password) && input$login_password == APP_PASSWORD) {
      authenticated(TRUE)
    } else {
      output$login_error <- renderUI({
        div(class = "error-message show",
            icon("exclamation-triangle"),
            " Contraseña incorrecta. Inténtalo de nuevo.")
      })
    }
  })
  
  # Permitir login con Enter
  observeEvent(input$login_password, {
    if (!is.null(input$login_password) && nchar(input$login_password) > 0) {
      output$login_error <- renderUI({
        NULL
      })
    }
  })
  
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    data_completo = NULL,
    text_data = NULL,
    percentage_results = NULL,
    normality_results = NULL,
    descriptive_results = NULL,
    correlation_results = NULL,
    comparative_results = NULL,
    fiabilidad_results = NULL,
    tabla1_results = NULL,
    composite_generated = FALSE,
    power_results = NULL,
    selected_vars_categorical = character(0),
    selected_vars_normality = character(0),
    selected_vars_plots = character(0),
    selected_vars_tables = character(0)
  )
  
  # Reactive values para IA
  ai_values <- reactiveValues(
    text_md = NULL,
    last_time = NULL
  )
  
  # Función reactiva para obtener el dataset activo
  active_data <- reactive({
    if (!is.null(values$data_completo)) {
      return(values$data_completo)
    } else {
      return(values$data)
    }
  })
  
  # Cargar datos
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      ext <- tools::file_ext(input$file$datapath)
      
      if (ext == "xlsx") {
        datos_raw <- read_excel(input$file$datapath)
        values$data <- limpiar_datos_encoding(datos_raw)
      } else if (ext == "csv") {
        datos_raw <- read.csv(input$file$datapath, fileEncoding = "UTF-8")
        values$data <- limpiar_datos_encoding(datos_raw)
      }
      
      showNotification("Archivo cargado exitosamente!", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error al cargar archivo:", e$message), type = "error", duration = 5)
    })
  })
  
  # Cargar archivo de texto
  observeEvent(input$text_file, {
    req(input$text_file)
    
    tryCatch({
      values$text_data <- readLines(input$text_file$datapath, encoding = "UTF-8")
      showNotification("Archivo de texto cargado exitosamente!", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error al cargar archivo de texto:", e$message), duration = 5)
    })
  })
  
  # ============================================================================
  # HANDLERS PARA PANEL DE BIENVENIDA
  # ============================================================================
  
  # Descargar archivo de datos de ejemplo (Excel)
  output$download_sample_data <- downloadHandler(
    filename = function() {
      "Data de prueba.xlsx"
    },
    content = function(file) {
      file.copy("Data de prueba.xlsx", file)
    }
  )
  
  # Descargar archivo de texto de ejemplo
  output$download_sample_text <- downloadHandler(
    filename = function() {
      "Estructura_tests.txt"
    },
    content = function(file) {
      file.copy("Estructura_tests.txt", file)
    }
  )
  
  # Botón para ir a cargar datos desde la página de bienvenida
  observeEvent(input$goto_upload, {
    updateTabItems(session, "sidebar_menu", selected = "upload")
  })
  
  # Output: indicador de archivo de texto cargado
  output$text_file_uploaded <- reactive({
    return(!is.null(values$text_data))
  })
  outputOptions(output, "text_file_uploaded", suspendWhenHidden = FALSE)
  
  # Output: vista previa del archivo de texto
  output$text_preview <- renderText({
    req(values$text_data)
    paste(head(values$text_data, 10), collapse = "\n")
  })
  
  output$file_uploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Generar variables compuestas
  observeEvent(input$generate_composite, {
    req(values$data, values$text_data)
    
    tryCatch({
      # Filtrar líneas de encabezado (variables sin items) antes de generar compuestas
      filtered_text <- filter_text_headers(values$text_data)
      generate_and_apply(values$data, filtered_text, new_name = "data_completo")
      
      if (exists("data_completo", envir = .GlobalEnv)) {
        values$data_completo <- get("data_completo", envir = .GlobalEnv)
        values$composite_generated <- TRUE
        showNotification(
          "Variables compuestas generadas exitosamente! Revisa la pestaña 'Visualización de Data' para ver tu base de datos completa.",
          type = "message",
          duration = 8
        )
      } else {
        showNotification("Error: No se pudo generar el dataset completo", type = "error", duration = 5)
      }
    }, error = function(e) {
      showNotification(paste("Error al generar variables compuestas:", e$message), type = "error", duration = 5)
    })
  })
  
  # Output: indicador de variables compuestas generadas
  output$composite_generated <- reactive({
    return(values$composite_generated)
  })
  outputOptions(output, "composite_generated", suspendWhenHidden = FALSE)
  
  # Output: resumen de variables compuestas
  output$composite_summary <- renderPrint({
    req(values$data_completo)
    cat("Dataset original:", ncol(values$data), "variables\n")
    cat("Dataset completo:", ncol(values$data_completo), "variables\n")
    cat("Columnas nuevas:", ncol(values$data_completo) - ncol(values$data), "\n\n")
    cat("Nombres de las columnas nuevas\n")
    nuevas_vars <- setdiff(names(values$data_completo), names(values$data))
    cat(paste(nuevas_vars, collapse = "\n"))
  })
  
  # Descarga del dataset completo
  output$download_composite <- downloadHandler(
    filename = function() {
      paste0(input$composite_dataset_name, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(values$data_completo)
      write.xlsx(values$data_completo, file, overwrite = TRUE)
    }
  )
  
  # Output: vista previa de datos
  output$data_preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Output: indicador de datos disponibles
  output$data_available <- reactive({
    return(!is.null(active_data()))
  })
  outputOptions(output, "data_available", suspendWhenHidden = FALSE)
  
  # Output: número total de filas
  output$total_rows <- renderText({
    req(active_data())
    nrow(active_data())
  })
  
  # Output: número total de columnas
  output$total_cols <- renderText({
    req(active_data())
    ncol(active_data())
  })
  
  # Output: tabla de datos completa
  output$full_data_table <- DT::renderDataTable({
    req(active_data())
    DT::datatable(
      active_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display nowrap",
      rownames = FALSE
    )
  })
  
  # Output: descargar base de datos completa
  output$download_full_data <- downloadHandler(
    filename = function() {
      paste0("ThesiStats_Data_Completa_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(active_data())
      write.xlsx(active_data(), file, overwrite = TRUE)
    }
  )
  
  # Función para crear botones de variables
  create_variable_buttons <- function(variables, selected_vars, section) {
    buttons <- lapply(variables, function(var) {
      is_selected <- var %in% selected_vars
      class_name <- if(is_selected) "variable-btn selected" else "variable-btn"
      
      tags$span(
        class = class_name,
        onclick = paste0("Shiny.setInputValue('", section, "_var_click', '", var, "', {priority: 'event'});"),
        var
      )
    })
    
    do.call(tagList, buttons)
  }
  
  # UI: botones de variables categóricas
  output$variable_buttons_categorical <- renderUI({
    req(values$data)
    character_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x))]
    create_variable_buttons(character_vars, values$selected_vars_categorical, "categorical")
  })
  
  # UI: botones de variables para normalidad
  output$variable_buttons_normality <- renderUI({
    req(active_data())
    # Filtrar solo variables cuantitativas continuas (numéricas con > 10 valores únicos)
    numeric_vars <- names(active_data())[sapply(active_data(), function(x) {
      is.numeric(x) && length(unique(na.omit(x))) > 10
    })]
    create_variable_buttons(numeric_vars, values$selected_vars_normality, "normality")
  })
  
  # UI: botones de variables para gráficos
  output$variable_buttons_plots <- renderUI({
    req(active_data())
    # Filtrar solo variables cuantitativas continuas (numéricas con > 10 valores únicos)
    numeric_vars <- names(active_data())[sapply(active_data(), function(x) {
      is.numeric(x) && length(unique(na.omit(x))) > 10
    })]
    create_variable_buttons(numeric_vars, values$selected_vars_plots, "plots")
  })
  
  # UI: botones de variables para tablas
  output$variable_buttons_tables <- renderUI({
    req(active_data())
    # Filtrar solo variables cuantitativas continuas (numéricas con > 10 valores únicos)
    numeric_vars <- names(active_data())[sapply(active_data(), function(x) {
      is.numeric(x) && length(unique(na.omit(x))) > 10
    })]
    create_variable_buttons(numeric_vars, values$selected_vars_tables, "tables")
  })
  
  # UI: variable de agrupación
  output$group_variable <- renderUI({
    req(active_data())
    factor_vars <- names(active_data())[sapply(active_data(), function(x) is.factor(x) || is.character(x))]
    selectInput("group_var", "Variable de agrupación:",
                choices = c("Ninguna" = "", factor_vars),
                selected = "")
  })
  
  # Manejo de clics en variables - Categóricas
  observeEvent(input$categorical_var_click, {
    var_name <- input$categorical_var_click
    if (var_name %in% values$selected_vars_categorical) {
      values$selected_vars_categorical <- setdiff(values$selected_vars_categorical, var_name)
    } else {
      values$selected_vars_categorical <- c(values$selected_vars_categorical, var_name)
    }
  })
  
  # Manejo de clics en variables - Normalidad
  observeEvent(input$normality_var_click, {
    var_name <- input$normality_var_click
    if (var_name %in% values$selected_vars_normality) {
      values$selected_vars_normality <- setdiff(values$selected_vars_normality, var_name)
    } else {
      values$selected_vars_normality <- c(values$selected_vars_normality, var_name)
    }
  })
  
  # Manejo de clics en variables - Gráficos
  observeEvent(input$plots_var_click, {
    var_name <- input$plots_var_click
    if (var_name %in% values$selected_vars_plots) {
      values$selected_vars_plots <- setdiff(values$selected_vars_plots, var_name)
    } else {
      values$selected_vars_plots <- c(values$selected_vars_plots, var_name)
    }
  })
  
  # Manejo de clics en variables - Tablas
  observeEvent(input$tables_var_click, {
    var_name <- input$tables_var_click
    if (var_name %in% values$selected_vars_tables) {
      values$selected_vars_tables <- setdiff(values$selected_vars_tables, var_name)
    } else {
      values$selected_vars_tables <- c(values$selected_vars_tables, var_name)
    }
  })
  
  # Botones de seleccionar/limpiar todas - Categóricas
  observeEvent(input$select_all_categorical, {
    req(values$data)
    character_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x))]
    values$selected_vars_categorical <- character_vars
  })
  
  observeEvent(input$clear_all_categorical, {
    values$selected_vars_categorical <- character(0)
  })
  
  # Botones de seleccionar/limpiar todas - Normalidad
  observeEvent(input$select_all_normality, {
    req(active_data())
    # Filtrar solo variables cuantitativas continuas (numéricas con > 10 valores únicos)
    numeric_vars <- names(active_data())[sapply(active_data(), function(x) {
      is.numeric(x) && length(unique(na.omit(x))) > 10
    })]
    values$selected_vars_normality <- numeric_vars
  })
  
  observeEvent(input$clear_all_normality, {
    values$selected_vars_normality <- character(0)
  })
  
  # Botones de seleccionar/limpiar todas - Gráficos
  observeEvent(input$select_all_plots, {
    req(active_data())
    # Filtrar solo variables cuantitativas continuas (numéricas con > 10 valores únicos)
    numeric_vars <- names(active_data())[sapply(active_data(), function(x) {
      is.numeric(x) && length(unique(na.omit(x))) > 10
    })]
    values$selected_vars_plots <- numeric_vars
  })
  
  observeEvent(input$clear_all_plots, {
    values$selected_vars_plots <- character(0)
  })
  
  # Botones de seleccionar/limpiar todas - Tablas
  observeEvent(input$select_all_tables, {
    req(active_data())
    # Filtrar solo variables cuantitativas continuas (numéricas con > 10 valores únicos)
    numeric_vars <- names(active_data())[sapply(active_data(), function(x) {
      is.numeric(x) && length(unique(na.omit(x))) > 10
    })]
    values$selected_vars_tables <- numeric_vars
  })
  
  observeEvent(input$clear_all_tables, {
    values$selected_vars_tables <- character(0)
  })
  
  # Output: contadores de variables seleccionadas
  output$selected_count_categorical <- renderText({
    count <- length(values$selected_vars_categorical)
    paste0("(", count, " variable", if(count != 1) "s" else "", ")")
  })
  
  output$selected_count_normality <- renderText({
    count <- length(values$selected_vars_normality)
    paste0("(", count, " variable", if(count != 1) "s" else "", ")")
  })
  
  output$selected_count_plots <- renderText({
    count <- length(values$selected_vars_plots)
    paste0("(", count, " variable", if(count != 1) "s" else "", ")")
  })
  
  output$selected_count_tables <- renderText({
    count <- length(values$selected_vars_tables)
    paste0("(", count, " variable", if(count != 1) "s" else "", ")")
  })
  
  # Cálculo de porcentajes
  observeEvent(input$calculate_percentages, {
    req(values$data, values$selected_vars_categorical)
    values$percentage_results <- calcular_porcentajes(values$data, values$selected_vars_categorical)
    showNotification("Porcentajes calculados!", duration = 3)
  })
  
  # Output: resultados de porcentajes
  output$percentage_results <- renderPrint({
    req(values$percentage_results)
    values$percentage_results
  })
  
  # Output: estadísticas de edad
  output$age_stats <- renderPrint({
    req(values$data, input$age_variable)
    edad_result <- edad_stat_safe(values$data, input$age_variable)
    # Si es un dataframe, redondear valores numéricos a 2 decimales
    if (is.data.frame(edad_result)) {
      numeric_cols <- sapply(edad_result, is.numeric)
      for (col in names(edad_result)[numeric_cols]) {
        edad_result[[col]] <- round(edad_result[[col]], 2)
      }
      edad_result
    } else {
      edad_result
    }
  })
  
  # Cálculo de potencia
  observeEvent(input$calculate_power, {
    result <- pwr.r.test(r = input$effect_size,
                         power = input$power_level,
                         sig.level = input$alpha_level,
                         alternative = "two.sided")
    
    values$power_results <- result
    
    output$power_results <- renderPrint({
      # Formatear salida con 2 decimales
      cat("     Approximate correlation power calculation (arcsine transformation)\n\n")
      cat(sprintf("              n = %.2f\n", result$n))
      cat(sprintf("              r = %.2f\n", result$r))
      cat(sprintf("      sig.level = %.2f\n", result$sig.level))
      cat(sprintf("          power = %.2f\n", result$power))
      cat(sprintf("    alternative = %s\n", result$alternative))
    })
  })
  
  # Pruebas de normalidad univariada
  observeEvent(input$test_normality, {
    req(active_data(), values$selected_vars_normality)
    values$normality_results <- normality_test_SW(active_data(), values$selected_vars_normality)
    showNotification("Pruebas de normalidad completadas!", duration = 3)
  })
  
  # Output: resultados de normalidad univariada
  output$normality_results <- DT::renderDataTable({
    req(values$normality_results)
    # Redondear columnas numéricas: 2 decimales en general, 3 decimales para p.value
    norm_table <- values$normality_results
    numeric_cols <- sapply(norm_table, is.numeric)
    for (col in names(norm_table)[numeric_cols]) {
      if (col == "p.value") {
        norm_table[[col]] <- round(norm_table[[col]], 3)
      } else {
        norm_table[[col]] <- round(norm_table[[col]], 2)
      }
    }
    DT::datatable(norm_table, options = list(scrollX = TRUE))
  })
  
  # Output: normalidad multivariada
  output$multivariate_plot <- renderPlot({
    req(active_data(), values$selected_vars_normality)
    
    tryCatch({
      df_sel <- active_data() %>% select(all_of(values$selected_vars_normality))
      
      # Validar que haya al menos 2 variables
      if (ncol(df_sel) < 2) {
        plot.new()
        text(0.5, 0.5, "Se requieren al menos 2 variables para el análisis de normalidad multivariada",
             cex = 1.2, col = "red")
        return(NULL)
      }
      
      # Validar que no haya varianza cero o multicolinealidad perfecta
      var_check <- apply(df_sel, 2, var, na.rm = TRUE)
      if (any(var_check == 0 | is.na(var_check))) {
        plot.new()
        text(0.5, 0.5, "Error: Una o más variables tienen varianza cero o valores faltantes.\nPor favor, revise sus datos.",
             cex = 1.2, col = "red")
        return(NULL)
      }
      
      p2 <- Multivariate_plot(
        df_sel,
        xmin = input$mv_xmin,
        xmax = input$mv_xmax,
        ymin = input$mv_ymin,
        ymax = input$mv_ymax
      )
      print(p2)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.6, "Error al generar el gráfico de normalidad multivariada",
           cex = 1.2, col = "red", font = 2)
      text(0.5, 0.4, paste("Detalles:", e$message),
           cex = 0.9, col = "darkred")
      text(0.5, 0.2, "Sugerencia: Verifique que las variables seleccionadas no tengan\nmulticolinealidad perfecta o varianza cero.",
           cex = 0.9, col = "darkblue")
    })
  })
  
  # Gráficos
  output$boxplots <- renderPlot({
    req(active_data(), values$selected_vars_plots)
    if (length(values$selected_vars_plots) > 0) {
      grafico_boxplots(active_data(), values$selected_vars_plots)
    }
  })
  
  # Tablas de resultados - botón para calcular
  observeEvent(input$calculate_tables, {
    req(active_data(), values$selected_vars_tables)
    
    if (length(values$selected_vars_tables) >= 2) {
      start_col <- values$selected_vars_tables[1]
      end_col <- values$selected_vars_tables[length(values$selected_vars_tables)]
      
      # Calcular descriptivos
      values$descriptive_results <- calculate_descriptives(active_data(), start_col, end_col)
      
      # Calcular correlaciones
      values$correlation_results <- calcular_correlaciones(
        active_data(),
        start_col,
        end_col,
        method = input$correlation_method,
        winsorize = input$winsorize_corr,
        show_pval = input$show_pval
      )
      
      # Calcular comparaciones por grupo si se seleccionó
      if (!is.null(input$group_var) && !is.na(input$group_var) && nzchar(input$group_var)) {
        tryCatch({
          # Validar que la variable de grupo tenga exactamente 2 niveles
          group_data <- active_data()[[input$group_var]]
          unique_levels <- unique(na.omit(group_data))
          
          if (length(unique_levels) != 2) {
            showNotification(
              paste0("Error: La variable de agrupación '", input$group_var,
                     "' debe tener exactamente 2 niveles. Actualmente tiene ",
                     length(unique_levels), " niveles: ", paste(unique_levels, collapse = ", ")),
              type = "error",
              duration = 8
            )
            values$comparative_results <- NULL
          } else {
            values$comparative_results <- Calcule_Comparative(
              active_data(),
              values$selected_vars_tables,
              group_var = input$group_var,
              Robust = input$robust_analysis
            )
          }
        }, error = function(e) {
          showNotification(
            paste("Error al calcular comparaciones por grupo:", e$message),
            type = "error",
            duration = 5
          )
          values$comparative_results <- NULL
        })
      }
      
      # Calcular fiabilidad y Tabla 1 si hay archivo de texto
      if (!is.null(values$text_data) && length(values$selected_vars_tables) >= 2) {
        # Filtrar texto antes de extraer items para evitar problemas con encabezados
        filtered_text <- filter_text_headers(values$text_data)
        extracted <- extract_items(filtered_text)
        values$fiabilidad_results <- calcula_omega_all(extracted, active_data())
        
        if (!is.null(values$descriptive_results) && !is.null(values$fiabilidad_results)) {
          values$tabla1_results <- inner_join(values$descriptive_results, values$fiabilidad_results, by = "Variables")
        }
      }
      
      showNotification("Tablas calculadas exitosamente!", type = "message", duration = 3)
    }
  })
  
  # Recalcular correlaciones cuando cambian los parámetros
  observeEvent(input$correlation_method, {
    req(active_data(), values$selected_vars_tables)
    if (length(values$selected_vars_tables) >= 2 && !is.null(values$correlation_results)) {
      start_col <- values$selected_vars_tables[1]
      end_col <- values$selected_vars_tables[length(values$selected_vars_tables)]
      
      values$correlation_results <- calcular_correlaciones(
        active_data(),
        start_col,
        end_col,
        method = input$correlation_method,
        winsorize = input$winsorize_corr,
        show_pval = input$show_pval
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$winsorize_corr, {
    req(active_data(), values$selected_vars_tables)
    if (length(values$selected_vars_tables) >= 2 && !is.null(values$correlation_results)) {
      start_col <- values$selected_vars_tables[1]
      end_col <- values$selected_vars_tables[length(values$selected_vars_tables)]
      
      values$correlation_results <- calcular_correlaciones(
        active_data(),
        start_col,
        end_col,
        method = input$correlation_method,
        winsorize = input$winsorize_corr,
        show_pval = input$show_pval
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_pval, {
    req(active_data(), values$selected_vars_tables)
    if (length(values$selected_vars_tables) >= 2 && !is.null(values$correlation_results)) {
      start_col <- values$selected_vars_tables[1]
      end_col <- values$selected_vars_tables[length(values$selected_vars_tables)]
      
      values$correlation_results <- calcular_correlaciones(
        active_data(),
        start_col,
        end_col,
        method = input$correlation_method,
        winsorize = input$winsorize_corr,
        show_pval = input$show_pval
      )
    }
  }, ignoreInit = TRUE)
  
  # Recalcular comparaciones cuando cambia el análisis robusto
  observeEvent(input$robust_analysis, {
    req(active_data(), values$selected_vars_tables)
    if (length(values$selected_vars_tables) >= 2 &&
        !is.null(values$comparative_results) &&
        !is.null(input$group_var) &&
        !is.na(input$group_var) &&
        nzchar(input$group_var)) {
      
      tryCatch({
        # Validar que la variable de grupo tenga exactamente 2 niveles
        group_data <- active_data()[[input$group_var]]
        unique_levels <- unique(na.omit(group_data))
        
        if (length(unique_levels) == 2) {
          values$comparative_results <- Calcule_Comparative(
            active_data(),
            values$selected_vars_tables,
            group_var = input$group_var,
            Robust = input$robust_analysis
          )
        }
      }, error = function(e) {
        showNotification(
          paste("Error al recalcular comparaciones:", e$message),
          type = "error",
          duration = 5
        )
      })
    }
  }, ignoreInit = TRUE)
  
  # Outputs de tablas
  output$correlation_table <- DT::renderDataTable({
    req(values$correlation_results)
    if (!is.null(values$correlation_results$correlation)) {
      cor_matrix <- values$correlation_results$correlation
      
      if (is.matrix(cor_matrix) || is.data.frame(cor_matrix)) {
        cor_matrix <- as.matrix(cor_matrix)
        
        # Solo redondear si todos los valores son numéricos (sin asteriscos)
        # Si show_pval es TRUE, los valores vienen con asteriscos (texto) y NO deben redondearse
        if (all(sapply(cor_matrix, is.numeric))) {
          cor_matrix <- round(cor_matrix, 2)
        }
        
        cor_df <- data.frame(
          Variable = rownames(cor_matrix),
          cor_matrix,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        rownames(cor_df) <- NULL
        
        DT::datatable(cor_df, options = list(scrollX = TRUE, pageLength = 15))
      }
    }
  })
  
  output$comparative_table <- DT::renderDataTable({
    req(values$comparative_results)
    # Redondear columnas numéricas: 2 decimales en general, 3 decimales para p
    comp_table <- values$comparative_results
    numeric_cols <- sapply(comp_table, is.numeric)
    for (col in names(comp_table)[numeric_cols]) {
      if (col == "p") {
        comp_table[[col]] <- round(comp_table[[col]], 3)
      } else {
        comp_table[[col]] <- round(comp_table[[col]], 2)
      }
    }
    DT::datatable(comp_table, options = list(scrollX = TRUE))
  })
  
  # Output: indicador de comparaciones disponibles
  output$comparative_available <- reactive({
    return(!is.null(values$comparative_results))
  })
  outputOptions(output, "comparative_available", suspendWhenHidden = FALSE)
  
  output$tabla1 <- DT::renderDataTable({
    req(values$tabla1_results)
    # Redondear todas las columnas numéricas a 2 decimales
    tabla1_display <- values$tabla1_results
    numeric_cols <- sapply(tabla1_display, is.numeric)
    for (col in names(tabla1_display)[numeric_cols]) {
      tabla1_display[[col]] <- round(tabla1_display[[col]], 2)
    }
    DT::datatable(tabla1_display, options = list(scrollX = TRUE))
  })
  
  # ===============================================
  # DESCARGAS - SECCIÓN UNIFICADA (SIN DUPLICADOS)
  # ===============================================
  
  # Excel - Normalidad (desde tab descargas)
  output$download_normality <- downloadHandler(
    filename = function() paste("Normalidad_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      req(values$normality_results)
      # Redondear: 2 decimales en general, 3 decimales para p.value
      norm_export <- values$normality_results
      numeric_cols <- sapply(norm_export, is.numeric)
      for (col in names(norm_export)[numeric_cols]) {
        if (col == "p.value") {
          norm_export[[col]] <- round(norm_export[[col]], 3)
        } else {
          norm_export[[col]] <- round(norm_export[[col]], 2)
        }
      }
      write.xlsx(norm_export, file)
    }
  )
  
  # Excel - Normalidad (desde tab normalidad)
  output$download_normality_table <- downloadHandler(
    filename = function() paste("Normalidad_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      req(values$normality_results)
      # Redondear: 2 decimales en general, 3 decimales para p.value
      norm_export <- values$normality_results
      numeric_cols <- sapply(norm_export, is.numeric)
      for (col in names(norm_export)[numeric_cols]) {
        if (col == "p.value") {
          norm_export[[col]] <- round(norm_export[[col]], 3)
        } else {
          norm_export[[col]] <- round(norm_export[[col]], 2)
        }
      }
      write.xlsx(norm_export, file)
    }
  )
  
  # Excel - Tabla 1 Descriptivos (desde tab descargas)
  output$download_descriptives <- downloadHandler(
    filename = function() paste("Tabla1_Descriptivos_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      req(values$tabla1_results)
      # Redondear a 2 decimales
      tabla1_export <- values$tabla1_results
      numeric_cols <- sapply(tabla1_export, is.numeric)
      for (col in names(tabla1_export)[numeric_cols]) {
        tabla1_export[[col]] <- round(tabla1_export[[col]], 2)
      }
      write.xlsx(tabla1_export, file)
    }
  )
  
  # Excel - Tabla 1 (desde tab tablas)
  output$download_tabla1 <- downloadHandler(
    filename = function() paste("Tabla1_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      req(values$tabla1_results)
      # Redondear a 2 decimales
      tabla1_export <- values$tabla1_results
      numeric_cols <- sapply(tabla1_export, is.numeric)
      for (col in names(tabla1_export)[numeric_cols]) {
        tabla1_export[[col]] <- round(tabla1_export[[col]], 2)
      }
      write.xlsx(tabla1_export, file)
    }
  )
  
  # Excel - Correlaciones (desde tab descargas)
  output$download_correlations <- downloadHandler(
    filename = function() paste("Tabla2_Correlaciones_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      req(values$correlation_results)
      req(values$correlation_results$correlation)
      cor_matrix <- values$correlation_results$correlation
      
      # Asegurar que sea matriz
      if (is.matrix(cor_matrix) || is.data.frame(cor_matrix)) {
        cor_matrix <- as.matrix(cor_matrix)
        
        # Redondear a 2 decimales solo si todas las columnas son numéricas
        if (all(sapply(cor_matrix, is.numeric))) {
          cor_matrix <- round(cor_matrix, 2)
        }
      }
      
      cor_df <- data.frame(
        Variable = rownames(cor_matrix),
        cor_matrix,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      write.xlsx(cor_df, file, rowNames = FALSE)
    }
  )
  
  # Excel - Correlaciones (desde tab tablas)
  output$download_correlation_matrix <- downloadHandler(
    filename = function() paste("Tabla2_Correlaciones_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      req(values$correlation_results)
      req(values$correlation_results$correlation)
      cor_matrix <- values$correlation_results$correlation
      
      # Asegurar que sea matriz
      if (is.matrix(cor_matrix) || is.data.frame(cor_matrix)) {
        cor_matrix <- as.matrix(cor_matrix)
        
        # Redondear a 2 decimales solo si todas las columnas son numéricas
        if (all(sapply(cor_matrix, is.numeric))) {
          cor_matrix <- round(cor_matrix, 2)
        }
      }
      
      cor_df <- data.frame(
        Variable = rownames(cor_matrix),
        cor_matrix,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      write.xlsx(cor_df, file, rowNames = FALSE)
    }
  )
  
  # Excel - Comparaciones (desde tab descargas)
  output$download_comparative <- downloadHandler(
    filename = function() paste("Tabla3_Comparaciones_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      req(values$comparative_results)
      # Redondear: 2 decimales en general, 3 decimales para p
      comp_export <- values$comparative_results
      numeric_cols <- sapply(comp_export, is.numeric)
      for (col in names(comp_export)[numeric_cols]) {
        if (col == "p") {
          comp_export[[col]] <- round(comp_export[[col]], 3)
        } else {
          comp_export[[col]] <- round(comp_export[[col]], 2)
        }
      }
      write.xlsx(comp_export, file)
    }
  )
  
  # Excel - Comparaciones (desde tab tablas)
  output$download_comparative_table <- downloadHandler(
    filename = function() paste("Tabla3_Comparaciones_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      req(values$comparative_results)
      # Redondear: 2 decimales en general, 3 decimales para p
      comp_export <- values$comparative_results
      numeric_cols <- sapply(comp_export, is.numeric)
      for (col in names(comp_export)[numeric_cols]) {
        if (col == "p") {
          comp_export[[col]] <- round(comp_export[[col]], 3)
        } else {
          comp_export[[col]] <- round(comp_export[[col]], 2)
        }
      }
      write.xlsx(comp_export, file)
    }
  )
  
  # JPG - Boxplot (desde tab descargas)
  output$download_boxplot <- downloadHandler(
    filename = function() paste("Boxplots_", Sys.Date(), ".jpg", sep = ""),
    content = function(file) {
      req(active_data(), values$selected_vars_plots)
      req(length(values$selected_vars_plots) > 0)
      p <- grafico_boxplots(active_data(), values$selected_vars_plots)
      ggsave(filename = file, plot = p, width = 10, height = 6, dpi = 600, device = "jpeg")
    }
  )
  
  # JPG - Boxplot (desde tab gráficos)
  output$download_boxplot_btn <- downloadHandler(
    filename = function() paste("Boxplots_", Sys.Date(), ".jpg", sep = ""),
    content = function(file) {
      req(active_data(), values$selected_vars_plots)
      req(length(values$selected_vars_plots) > 0)
      p <- grafico_boxplots(active_data(), values$selected_vars_plots)
      ggsave(filename = file, plot = p, width = 10, height = 6, dpi = 600, device = "jpeg")
    }
  )
  
  # JPG - Normalidad Multivariada (desde tab descargas)
  output$download_multivariate_plot <- downloadHandler(
    filename = function() paste0("Normalidad_Multivariada_", Sys.Date(), ".jpg"),
    content = function(file) {
      req(active_data(), values$selected_vars_normality)
      df_sel <- active_data() %>% select(all_of(values$selected_vars_normality))
      p2 <- Multivariate_plot(
        df_sel,
        xmin = input$mv_xmin,
        xmax = input$mv_xmax,
        ymin = input$mv_ymin,
        ymax = input$mv_ymax
      )
      ggsave(filename = file, plot = p2, height = 5, width = 8.5, dpi = 600, device = "jpeg")
    }
  )
  
  # JPG - Normalidad Multivariada (desde tab normalidad)
  output$download_multivariate_plot_btn <- downloadHandler(
    filename = function() paste0("Normalidad_Multivariada_", Sys.Date(), ".jpg"),
    content = function(file) {
      req(active_data(), values$selected_vars_normality)
      df_sel <- active_data() %>% select(all_of(values$selected_vars_normality))
      p2 <- Multivariate_plot(
        df_sel,
        xmin = input$mv_xmin,
        xmax = input$mv_xmax,
        ymin = input$mv_ymin,
        ymax = input$mv_ymax
      )
      ggsave(filename = file, plot = p2, height = 5, width = 8.5, dpi = 600, device = "jpeg")
    }
  )
  
  # DOCX - Reporte completo Word
  output$download_word_report <- downloadHandler(
    filename = function() paste0("Reporte_ThesiStats_", Sys.Date(), ".docx"),
    content = function(file) {
      # Crear archivos temporales para las imágenes
      temp_dir <- tempdir()
      
      # Guardar gráficos temporales
      fig_box_path <- NULL
      fig_mvnorm_path <- NULL
      
      if (!is.null(values$selected_vars_plots) && length(values$selected_vars_plots) > 0) {
        fig_box_path <- file.path(temp_dir, "boxplots_temp.jpg")
        p_box <- grafico_boxplots(active_data(), values$selected_vars_plots)
        ggsave(filename = fig_box_path, plot = p_box, width = 10, height = 6, dpi = 600, device = "jpeg")
      }
      
      if (!is.null(values$selected_vars_normality) && length(values$selected_vars_normality) > 0) {
        fig_mvnorm_path <- file.path(temp_dir, "mvnorm_temp.jpg")
        df_sel <- active_data() %>% select(all_of(values$selected_vars_normality))
        p_mvnorm <- Multivariate_plot(
          df_sel,
          xmin = input$mv_xmin,
          xmax = input$mv_xmax,
          ymin = input$mv_ymin,
          ymax = input$mv_ymax
        )
        ggsave(filename = fig_mvnorm_path, plot = p_mvnorm, height = 5, width = 8.5, dpi = 600, device = "jpeg")
      }
      
      # Preparar datos de correlaciones
      tabla2_data <- NULL
      if (!is.null(values$correlation_results) && !is.null(values$correlation_results$correlation)) {
        cor_matrix <- values$correlation_results$correlation
        tabla2_data <- data.frame(
          Variable = rownames(cor_matrix),
          cor_matrix,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
      
      # Construir texto de participantes elaborado
      participantes_texto <- "No se completaron todos los análisis de participantes. Por favor, calcule los porcentajes en la pestaña 'Participantes' y el análisis de potencia."
      
      if (!is.null(values$percentage_results)) {
        # Extraer N total
        n_total <- nrow(values$data)
        
        # Construir el texto principal
        texto_partes <- paste0("Participaron ", n_total, " participantes")
        
        # Agregar estadísticas de edad si están disponibles
        edad_texto <- ""
        if (!is.null(input$age_variable) && input$age_variable != "" &&
            input$age_variable %in% names(values$data)) {
          edad_data <- values$data[[input$age_variable]]
          edad_data <- edad_data[!is.na(edad_data)]
          if (length(edad_data) > 0) {
            edad_min <- min(edad_data, na.rm = TRUE)
            edad_max <- max(edad_data, na.rm = TRUE)
            edad_M <- round(mean(edad_data, na.rm = TRUE), 2)
            edad_DE <- round(sd(edad_data, na.rm = TRUE), 2)
            edad_texto <- paste0(", con edades entre ", edad_min, " y ", edad_max,
                                 " años (M = ", edad_M, ", DE = ", edad_DE, ")")
          }
        }
        
        participantes_texto <- paste0(texto_partes, edad_texto, ". ")
        
        # Agregar análisis de potencia si está disponible
        if (!is.null(values$power_results)) {
          n_minimo <- ceiling(values$power_results$n)
          participantes_texto <- paste0(
            participantes_texto,
            "Para evaluar la adecuación muestral, a partir del tamaño de efecto promedio absoluto observado (r = ",
            input$effect_size,
            ") se empleó el paquete 'pwr' (Champely, 2020): con potencia = ",
            input$power_level,
            ", α = ",
            input$alpha_level,
            " y contraste bilateral, el mínimo requerido fue de aproximadamente ",
            n_minimo,
            " participantes, por lo que la muestra disponible resultó ",
            ifelse(n_total >= n_minimo, "suficiente", "insuficiente"),
            ". "
          )
        }
        
        # Agregar información de variables categóricas si están disponibles
        if (!is.null(values$percentage_results)) {
          porcentajes_texto <- capture.output(print(values$percentage_results))
          porcentajes_texto <- paste(porcentajes_texto, collapse = "\n")
          participantes_texto <- paste0(participantes_texto, "\n\nDistribución de variables categóricas:\n", porcentajes_texto)
        }
      }
      
      # Generar reporte
      build_thesistats_report(
        outfile = file,
        participantes_texto = participantes_texto,
        tabla1 = values$tabla1_results,
        tabla2 = tabla2_data,
        tabla3 = values$comparative_results,
        fig_cor_jpg = NULL,
        fig_box_jpg = fig_box_path,
        fig_mvnorm_jpg = fig_mvnorm_path
      )
      
      showNotification("Reporte Word generado exitosamente!", type = "message", duration = 3)
    }
  )
  
  # ===============================================
  # GENERACIÓN DE REPORTE CON IA (CHATGPT)
  # ===============================================
  
  observeEvent(input$ai_generate_btn, {
    # Validaciones
    if (is.null(values$percentage_results) && is.null(values$tabla1_results)) {
      showNotification("Primero realiza los análisis de participantes y tablas.", type = "warning", duration = 5)
      return()
    }
    if (!nzchar(input$ai_api_key)) {
      showNotification("Ingresa una API key válida de OpenAI (sk-...).", type = "error", duration = 5)
      return()
    }
    
    withProgress(message = "Contactando a ChatGPT...", value = 0, {
      incProgress(0.2, detail = "Preparando contexto...")
      
      # ==== RECOPILAR INFORMACIÓN ====
      
      # 1. Información de participantes
      participantes_info <- if (!is.null(values$percentage_results)) {
        paste(capture.output(print(values$percentage_results)), collapse = "\n")
      } else "No disponible"
      
      # 2. Estadísticas de edad
      edad_info <- if (!is.null(input$age_variable) && input$age_variable != "" &&
                       !is.null(values$data) && input$age_variable %in% names(values$data)) {
        paste(capture.output(edad_stat_safe(values$data, input$age_variable)), collapse = "\n")
      } else "No disponible"
      
      # 3. Análisis de potencia
      power_info <- if (!is.null(values$power_results)) {
        sprintf("Tamaño de efecto (r) = %.2f\nPotencia = %.2f\nAlfa = %.2f\nN mínimo requerido = %d\nN actual = %d",
                input$effect_size, input$power_level, input$alpha_level,
                ceiling(values$power_results$n), nrow(values$data))
      } else "No disponible"
      
      # 4. Resultados de normalidad
      normality_info <- if (!is.null(values$normality_results)) {
        paste(capture.output(print(values$normality_results)), collapse = "\n")
      } else "No disponible"
      
      # 5. Descriptivos y fiabilidad (Tabla 1)
      tabla1_info <- if (!is.null(values$tabla1_results)) {
        paste(capture.output(print(values$tabla1_results)), collapse = "\n")
      } else "No disponible"
      
      # 6. Correlaciones (Tabla 2)
      tabla2_info <- if (!is.null(values$correlation_results) &&
                         !is.null(values$correlation_results$correlation)) {
        cor_matrix <- values$correlation_results$correlation
        
        # Si hay archivo de texto con estructura de variables, filtrar correlaciones relevantes
        if (!is.null(values$text_data)) {
          tryCatch({
            # Crear mapeo de dimensiones a variables
            dim_var_map <- create_dimension_variable_map(values$text_data)
            
            # Filtrar correlaciones relevantes (entre dimensiones de diferentes variables)
            relevant_cors <- filter_relevant_correlations(cor_matrix, dim_var_map)
            
            # Formatear para el prompt
            if (length(relevant_cors) > 0) {
              format_relevant_correlations(relevant_cors)
            } else {
              # Si no hay correlaciones relevantes, mostrar la matriz completa
              paste(capture.output(print(cor_matrix)), collapse = "\n")
            }
          }, error = function(e) {
            # Si hay error, usar el formato original
            paste(capture.output(print(cor_matrix)), collapse = "\n")
          })
        } else {
          # Si no hay archivo de texto, usar el formato original
          paste(capture.output(print(cor_matrix)), collapse = "\n")
        }
      } else "No disponible"
      
      # 7. Comparaciones por grupo (Tabla 3)
      tabla3_info <- if (!is.null(values$comparative_results)) {
        paste(capture.output(print(values$comparative_results)), collapse = "\n")
      } else "No disponible"
      
      incProgress(0.3, detail = "Construyendo prompt...")
      
      # ==== CONSTRUIR PROMPT ====
      
      system_inst <- if (input$ai_lang == "Español") {
        "Eres un experto en redacción académica de investigación en psicología y estadística. Escribes reportes académicos siguiendo estrictamente el formato APA 7, con lenguaje formal, técnico y claro, similar a artículos publicados en revistas científicas. Integras citas bibliográficas en cada afirmación metodológica y usas nomenclatura estadística apropiada (M, DE, rw, d, ω, g2, α)."
      } else {
        "You are an expert in academic writing for psychology and statistics research. You write academic reports strictly following APA 7 format, with formal, technical and clear language, similar to articles published in scientific journals. You integrate bibliographic citations in each methodological statement and use appropriate statistical nomenclature (M, SD, rw, d, ω, g2, α)."
      }
      
      user_message <- if (input$ai_lang == "Español") {
        paste(
          "# INFORMACIÓN DEL ESTUDIO",
          "",
          "## Participantes",
          participantes_info,
          "",
          "## Edad",
          edad_info,
          "",
          "## Análisis de Potencia Estadística",
          power_info,
          "",
          "## Normalidad (Shapiro-Wilk)",
          normality_info,
          "",
          "## Tabla 1: Descriptivos y Fiabilidad (ω)",
          tabla1_info,
          "",
          "## Tabla 2: Correlaciones",
          tabla2_info,
          "",
          "## Tabla 3: Comparaciones por Grupo",
          tabla3_info,
          "",
          "---",
          "",
          "## TU TAREA:",
          "Escribe un reporte académico completo (2000-2500 palabras) siguiendo EXACTAMENTE este formato:",
          "",
          "**Método**",
          "",
          "**Participantes**",
          "Escribe un párrafo descriptivo que incluya: número total de participantes, distribución por sexo, rango y estadísticos de edad (M, DE). Integra el análisis de potencia en el MISMO párrafo mencionando: tamaño de efecto observado, potencia, alfa, N mínimo requerido y conclusión sobre adecuación muestral. Si hay datos adicionales (ej. tiempo en redes), descríbelos. Finaliza con criterios de inclusión. Incluye citas pertinentes (ej. paquete 'pwr').",
          "",
          "**Análisis de datos**",
          "Escribe 1-2 párrafos describiendo: (1) Software usado con cita, (2) Prueba de normalidad univariada con cita y mención al 'Apéndice A', (3) Identificación de atípicos con diagramas de caja y mención al 'Apéndice B', (4) Normalidad multivariada con mención al 'Apéndice C', (5) Justificación de métodos robustos si hay desviaciones (ej. correlación winsorizada) con citas, (6) Criterios de interpretación de tamaños de efecto según Cohen (1988): pequeños (r ≥ .10, d ≥ 0.20), medianos (r ≥ .30, d ≥ 0.50) y grandes (r ≥ .50, d ≥ 0.80). Usa nomenclatura: M, DE, rw, d, ω, g2, α.",
          "",
          "**Resultados**",
          "",
          "Escribe 3 párrafos interpretativos (uno por tabla):",
          "",
          "Párrafo 1: Describe los estadísticos descriptivos y fiabilidad. Menciona variables con medias más altas y más bajas, interpreta asimetría/curtosis, reporta coeficientes omega. Finaliza con '[Insertar Tabla 1]'.",
          "",
          "Párrafo 2: Describe ÚNICAMENTE las correlaciones entre dimensiones de DIFERENTES instrumentos/pruebas (NO describas correlaciones entre dimensiones del mismo instrumento, por ejemplo NO menciones correlaciones entre EDS_Dim1 y EDS_Dim2). Al interpretar magnitudes, APLICA CORRECTAMENTE los criterios de Cohen (1988): pequeño (.10 ≤ r < .30), mediano (.30 ≤ r < .50), GRANDE (r ≥ .50). Por ejemplo: r = 0.52 es GRANDE, r = 0.53 es GRANDE, r = 0.45 es mediano, r = 0.25 es pequeño. Identifica patrones teóricamente relevantes. Finaliza con '[Insertar Tabla 2]'.",
          "",
          "Párrafo 3: Describe las comparaciones por grupo, reporta medias y tamaños de efecto (d), interpreta diferencias. Finaliza con '[Insertar Tabla 3]'.",
          "",
          "**Referencias**",
          "Incluye mínimo 6-8 referencias en formato APA 7 completo con DOIs. Incluye: paquetes R usados, pruebas estadísticas citadas, criterios de interpretación, y métodos robustos mencionados.",
          "",
          "IMPORTANTE:",
          "- NO incluyas título de investigación. Comienza directo con **Método**",
          "- Usa [Insertar Tabla 1], [Insertar Tabla 2], [Insertar Tabla 3]",
          "- Menciona Apéndice A, B, C donde corresponda",
          "- Integra TODAS las cifras numéricas proporcionadas",
          "- Escribe de forma fluida y académica, no como lista de puntos",
          sep = "\n"
        )
      } else {
        paste(
          "# STUDY INFORMATION",
          "",
          "## Participants",
          participantes_info,
          "",
          "## Age",
          edad_info,
          "",
          "## Statistical Power Analysis",
          power_info,
          "",
          "## Normality (Shapiro-Wilk)",
          normality_info,
          "",
          "## Table 1: Descriptives and Reliability (ω)",
          tabla1_info,
          "",
          "## Table 2: Correlations",
          tabla2_info,
          "",
          "## Table 3: Group Comparisons",
          tabla3_info,
          "",
          "---",
          "",
          "## YOUR TASK:",
          "Write a complete academic report (2000-2500 words) following EXACTLY this format:",
          "",
          "**Method**",
          "",
          "**Participants**",
          "Write a descriptive paragraph including: total number of participants, sex distribution, age range and statistics (M, SD). Integrate power analysis in the SAME paragraph mentioning: observed effect size, power, alpha, minimum required N, and conclusion about sample adequacy. If additional data exists (e.g., social media time), describe it. End with inclusion criteria. Include relevant citations (e.g., 'pwr' package).",
          "",
          "**Data Analysis**",
          "Write 1-2 paragraphs describing: (1) Software used with citation, (2) Univariate normality test with citation and mention of 'Appendix A', (3) Outlier identification with boxplots and mention of 'Appendix B', (4) Multivariate normality with mention of 'Appendix C', (5) Justification of robust methods if deviations exist (e.g., winsorized correlation) with citations, (6) Effect size interpretation criteria according to Cohen (1988): small (r ≥ .10, d ≥ 0.20), medium (r ≥ .30, d ≥ 0.50), and large (r ≥ .50, d ≥ 0.80). Use nomenclature: M, SD, rw, d, ω, g2, α.",
          "",
          "**Results**",
          "",
          "Write 3 interpretive paragraphs (one per table):",
          "",
          "Paragraph 1: Describe descriptive statistics and reliability. Mention variables with highest and lowest means, interpret skewness/kurtosis, report omega coefficients. End with '[Insert Table 1]'.",
          "",
          "Paragraph 2: Describe ONLY correlations between dimensions from DIFFERENT instruments/tests (DO NOT describe correlations between dimensions of the same instrument, for example DO NOT mention correlations between EDS_Dim1 and EDS_Dim2). When interpreting magnitudes, CORRECTLY APPLY Cohen's (1988) criteria: small (.10 ≤ r < .30), medium (.30 ≤ r < .50), LARGE (r ≥ .50). For example: r = 0.52 is LARGE, r = 0.53 is LARGE, r = 0.45 is medium, r = 0.25 is small. Identify theoretically relevant patterns. End with '[Insert Table 2]'.",
          "",
          "Paragraph 3: Describe group comparisons, report means and effect sizes (d), interpret differences. End with '[Insert Table 3]'.",
          "",
          "**References**",
          "Include minimum 6-8 references in complete APA 7 format with DOIs. Include: R packages used, cited statistical tests, interpretation criteria, and mentioned robust methods.",
          "",
          "IMPORTANT:",
          "- DO NOT include research title. Start directly with **Method**",
          "- Use [Insert Table 1], [Insert Table 2], [Insert Table 3]",
          "- Mention Appendix A, B, C where appropriate",
          "- Integrate ALL numerical figures provided",
          "- Write fluently and academically, not as bullet points",
          sep = "\n"
        )
      }
      
      incProgress(0.4, detail = "Enviando a OpenAI...")
      
      # ==== LLAMADA A OPENAI API ====
      req <- request("https://api.openai.com/v1/chat/completions") |>
        req_headers(
          Authorization = paste("Bearer", input$ai_api_key),
          "Content-Type" = "application/json"
        ) |>
        req_body_json(list(
          model = "gpt-4.1",
          temperature = 0.3,
          max_tokens = 6000,
          messages = list(
            list(role = "system", content = system_inst),
            list(role = "user", content = user_message)
          )
        ))
      
      resp <- tryCatch(req_perform(req), error = function(e) e)
      
      if (inherits(resp, "error")) {
        showNotification(paste("Error de OpenAI:", resp$message), type = "error", duration = 10)
        return()
      }
      
      incProgress(0.8, detail = "Procesando respuesta...")
      
      js <- tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
      
      if (is.null(js)) {
        showNotification("Respuesta JSON inválida", type = "error", duration = 10)
        return()
      }
      if (!is.null(js$error)) {
        showNotification(paste("Error de API:", js$error$message), type = "error", duration = 10)
        return()
      }
      
      txt <- ""
      if (!is.null(js$choices) && length(js$choices) > 0) {
        if (!is.null(js$choices[[1]]$message$content)) {
          txt <- js$choices[[1]]$message$content
        }
      }
      
      if (!nzchar(txt)) {
        showNotification("Respuesta vacía de la API", type = "error", duration = 10)
        return()
      }
      
      ai_values$text_md <- txt
      ai_values$last_time <- Sys.time()
      
      html <- try(commonmark::markdown_html(ai_values$text_md), silent = TRUE)
      if (inherits(html, "try-error")) {
        html <- paste0("<pre>", htmltools::htmlEscape(ai_values$text_md), "</pre>")
      }
      output$ai_report_html <- renderUI(HTML(html))
      
      incProgress(1.0, detail = "¡Completado!")
      showNotification("¡Reporte generado exitosamente!", type = "message", duration = 5)
    })
  })
  
  # Función auxiliar para convertir markdown a formato Word
  parse_markdown_to_word <- function(text) {
    # Función para procesar una línea y convertir **texto** en negritas
    process_bold <- function(line_text) {
      parts <- list()
      remaining <- line_text
      
      while(nchar(remaining) > 0) {
        # Buscar patrón **texto**
        bold_match <- regexpr("\\*\\*[^*]+\\*\\*", remaining)
        
        if (bold_match[1] > 0) {
          # Hay texto antes del bold
          if (bold_match[1] > 1) {
            before_text <- substr(remaining, 1, bold_match[1] - 1)
            parts[[length(parts) + 1]] <- ftext(before_text)
          }
          
          # Extraer texto en negrita (sin los **)
          bold_length <- attr(bold_match, "match.length")
          bold_full <- substr(remaining, bold_match[1], bold_match[1] + bold_length - 1)
          bold_text <- gsub("^\\*\\*|\\*\\*$", "", bold_full)
          parts[[length(parts) + 1]] <- ftext(bold_text, prop = fp_text(bold = TRUE))
          
          # Actualizar remaining
          remaining <- substr(remaining, bold_match[1] + bold_length, nchar(remaining))
        } else {
          # No hay más negritas, agregar el resto
          if (nchar(remaining) > 0) {
            parts[[length(parts) + 1]] <- ftext(remaining)
          }
          break
        }
      }
      
      if (length(parts) == 0) {
        return(fpar(ftext(line_text)))
      }
      
      do.call(fpar, parts)
    }
    
    return(process_bold(text))
  }
  
  # Descargar reporte en DOCX
  output$ai_download_docx <- downloadHandler(
    filename = function() paste0("ThesiStats_Reporte_IA_", format(Sys.Date()), ".docx"),
    content = function(file) {
      req(ai_values$text_md)
      
      tryCatch({
        doc <- read_docx()
        
        # Título
        doc <- body_add_par(doc, "THESISTATS - REPORTE GENERADO CON IA", style = "heading 1")
        doc <- body_add_par(doc, paste("Generado el:", format(Sys.Date(), "%d/%m/%Y")), style = "Normal")
        doc <- body_add_par(doc, "", style = "Normal")
        
        # Contenido en markdown
        lines <- strsplit(ai_values$text_md, "\n")[[1]]
        for (line in lines) {
          if (grepl("^##\\s+", line)) {
            # Encabezado 2
            heading_text <- gsub("^##\\s+", "", line)
            heading_text <- gsub("\\*\\*", "", heading_text)  # Remover ** de encabezados
            doc <- body_add_par(doc, heading_text, style = "heading 2")
          } else if (grepl("^#\\s+", line)) {
            # Encabezado 1
            heading_text <- gsub("^#\\s+", "", line)
            heading_text <- gsub("\\*\\*", "", heading_text)  # Remover ** de encabezados
            doc <- body_add_par(doc, heading_text, style = "heading 1")
          } else if (nzchar(trimws(line))) {
            # Párrafo normal con procesamiento de negritas
            if (grepl("\\*\\*", line)) {
              doc <- body_add_fpar(doc, parse_markdown_to_word(line))
            } else {
              doc <- body_add_par(doc, line, style = "Normal")
            }
          } else {
            doc <- body_add_par(doc, "", style = "Normal")
          }
        }
        
        print(doc, target = file)
      }, error = function(e) {
        showNotification(paste("Error al crear DOCX:", e$message), type = "error", duration = 5)
      })
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)


