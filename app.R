showNotification("Tablas calculadas exitosamente!", duration = 3)# Cargar librerías necesarias
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

# CSS personalizado para los botones de variables
css <- "
.variable-btn {
  margin: 3px;
  padding: 8px 12px;
  border: 1px solid #ddd;
  border-radius: 20px;
  background-color: #f8f9fa;
  color: #495057;
  cursor: pointer;
  display: inline-block;
  transition: all 0.3s ease;
}

.variable-btn:hover {
  background-color: #e9ecef;
  border-color: #adb5bd;
}

.variable-btn.selected {
  background-color: #007bff;
  color: white;
  border-color: #007bff;
}

.variable-container {
  border: 1px solid #ddd;
  border-radius: 5px;
  padding: 10px;
  min-height: 60px;
  background-color: #ffffff;
}

.section-title {
  font-weight: bold;
  margin-bottom: 10px;
  color: #495057;
}
"

# Las funciones vienen de la librería ThesiStats
# No es necesario definirlas aquí ya que están en el paquete

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Datos - Shiny App"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Cargar Datos", tabName = "upload", icon = icon("upload")),
      menuItem("Cargar Archivo Texto", tabName = "upload_text", icon = icon("file-text")),
      menuItem("Descripción de Participantes", tabName = "description", icon = icon("users")),
      menuItem("Análisis de Potencia", tabName = "power", icon = icon("calculator")),
      menuItem("Normalidad", tabName = "normality", icon = icon("chart-line")),
      menuItem("Gráficos", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Tablas de Resultados", tabName = "tables", icon = icon("table")),
      menuItem("Descargas", tabName = "downloads", icon = icon("download"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML(css))),
    tabItems(
      # Tab 1: Cargar datos
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Cargar Archivo", status = "primary", solidHeader = TRUE, width = 12,
                  fileInput("file", "Selecciona un archivo Excel (.xlsx) o CSV (.csv)",
                            accept = c(".xlsx", ".csv")),
                  hr(),
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    h4("Vista previa de los datos:"),
                    DT::dataTableOutput("data_preview")
                  )
                )
              )
      ),

      # Tab 1.5: Cargar archivo de texto
      tabItem(tabName = "upload_text",
              fluidRow(
                box(
                  title = "Cargar Archivo de Texto", status = "primary", solidHeader = TRUE, width = 12,
                  fileInput("text_file", "Selecciona un archivo de texto (.txt)",
                            accept = c(".txt")),
                  hr(),
                  conditionalPanel(
                    condition = "output.text_file_uploaded",
                    h4("Vista previa del archivo de texto:"),
                    verbatimTextOutput("text_preview")
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
                  numericInput("effect_size", "Tamaño del efecto (r):", value = 0.27, min = 0, max = 1, step = 0.01),
                  numericInput("power_level", "Potencia deseada:", value = 0.80, min = 0, max = 1, step = 0.01),
                  numericInput("alpha_level", "Nivel de significancia:", value = 0.05, min = 0, max = 1, step = 0.01),
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
                  title = "Boxplots", status = "primary", solidHeader = TRUE, width = 12,
                  plotOutput("boxplots", height = "500px"),
                  br(),
                  downloadButton("download_boxplot", "Descargar Boxplot (JPG)", class = "btn-success")
                )
              ),
              fluidRow(
                box(
                  title = "Gráfico de Correlaciones", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("correlation_plot"),
                  br(),
                  downloadButton("download_correlation", "Descargar Correlación (JPG)", class = "btn-success")
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
                  br(),
                  fluidRow(
                    column(6,
                           checkboxInput("robust_analysis", "Análisis robusto", TRUE)
                    ),
                    column(6,
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
                  DT::dataTableOutput("correlation_table"),
                  br(),
                  downloadButton("download_correlation_matrix", "Descargar Tabla 2 (Excel)", class = "btn-success")
                )
              ),
              fluidRow(
                box(
                  title = "Tabla 3: Comparaciones por Grupo", status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("comparative_table"),
                  br(),
                  downloadButton("download_comparative", "Descargar Tabla 3 (Excel)", class = "btn-success")
                )
              )
      ),

      # Tab 7: Descargas
      tabItem(tabName = "downloads",
              fluidRow(
                box(
                  title = "Descargar Resultados", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Descargar archivos de resultados:"),
                  br(),
                  downloadButton("download_normality", "Descargar Normalidad (Excel)", class = "btn-success"),
                  br(), br(),
                  downloadButton("download_descriptives", "Descargar Descriptivos (Excel)", class = "btn-info"),
                  br(), br(),
                  downloadButton("download_correlations", "Descargar Correlaciones (Excel)", class = "btn-warning")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    text_data = NULL,
    percentage_results = NULL,
    normality_results = NULL,
    descriptive_results = NULL,
    correlation_results = NULL,
    comparative_results = NULL,
    fiabilidad_results = NULL,
    tabla1_results = NULL,
    selected_vars_categorical = character(0),
    selected_vars_normality = character(0),
    selected_vars_plots = character(0),
    selected_vars_tables = character(0)
  )

  # Cargar datos
  observeEvent(input$file, {
    req(input$file)

    tryCatch({
      ext <- tools::file_ext(input$file$datapath)

      if (ext == "xlsx") {
        values$data <- read_excel(input$file$datapath)
      } else if (ext == "csv") {
        values$data <- read.csv(input$file$datapath)
      }

      showNotification("Archivo cargado exitosamente!", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error al cargar archivo:", e$message), duration = 5)
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

  # Output: vista previa de datos
  output$data_preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, options = list(scrollX = TRUE, pageLength = 10))
  })

  # UI: variables categóricas
  output$categorical_vars <- renderUI({
    req(values$data)
    character_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x))]
    checkboxGroupInput("selected_categorical", "Selecciona variables categóricas:",
                       choices = character_vars,
                       selected = character_vars[1:min(5, length(character_vars))])
  })

  # UI: variable de edad
  output$age_var <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("age_variable", "Selecciona variable de edad:",
                choices = numeric_vars,
                selected = if("Edad" %in% numeric_vars) "Edad" else numeric_vars[1])
  })

  # UI: botones de variables categóricas
  output$variable_buttons_categorical <- renderUI({
    req(values$data)
    character_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x))]
    create_variable_buttons(character_vars, values$selected_vars_categorical, "categorical")
  })

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

  # UI: botones de variables para normalidad
  output$variable_buttons_normality <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    create_variable_buttons(numeric_vars, values$selected_vars_normality, "normality")
  })

  # UI: botones de variables para gráficos
  output$variable_buttons_plots <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    create_variable_buttons(numeric_vars, values$selected_vars_plots, "plots")
  })

  # UI: botones de variables para tablas
  output$variable_buttons_tables <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    create_variable_buttons(numeric_vars, values$selected_vars_tables, "tables")
  })

  # UI: variable de agrupación
  output$group_variable <- renderUI({
    req(values$data)
    factor_vars <- names(values$data)[sapply(values$data, function(x) is.factor(x) || is.character(x))]
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

  # Botones de seleccionar/limpiar todas - Categóricas
  observeEvent(input$select_all_categorical, {
    req(values$data)
    character_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x))]
    values$selected_vars_categorical <- character_vars
  })

  observeEvent(input$clear_all_categorical, {
    values$selected_vars_categorical <- character(0)
  })
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

  # Botones de seleccionar/limpiar todas - Normalidad
  observeEvent(input$select_all_normality, {
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    values$selected_vars_normality <- numeric_vars
  })

  observeEvent(input$clear_all_normality, {
    values$selected_vars_normality <- character(0)
  })

  # Botones de seleccionar/limpiar todas - Gráficos
  observeEvent(input$select_all_plots, {
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    values$selected_vars_plots <- numeric_vars
  })

  observeEvent(input$clear_all_plots, {
    values$selected_vars_plots <- character(0)
  })

  # Botones de seleccionar/limpiar todas - Tablas
  observeEvent(input$select_all_tables, {
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    values$selected_vars_tables <- numeric_vars
  })

  observeEvent(input$clear_all_tables, {
    values$selected_vars_tables <- character(0)
  })

  # Output: contadores de variables seleccionadas
  output$selected_count_categorical <- renderText({
    paste0("(", length(values$selected_vars_categorical), " variables)")
  })

  output$selected_count_normality <- renderText({
    paste0("(", length(values$selected_vars_normality), " variables)")
  })

  output$selected_count_plots <- renderText({
    paste0("(", length(values$selected_vars_plots), " variables)")
  })

  output$selected_count_tables <- renderText({
    paste0("(", length(values$selected_vars_tables), " variables)")
  })
  observeEvent(input$calculate_percentages, {
    req(values$data, input$selected_categorical)
    values$percentage_results <- calcular_porcentajes(values$data, input$selected_categorical)
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
    edad_stat(values$data, input$age_variable)
  })

  # Cálculo de potencia
  observeEvent(input$calculate_power, {
    result <- pwr.r.test(r = input$effect_size,
                         power = input$power_level,
                         sig.level = input$alpha_level,
                         alternative = "two.sided")

    output$power_results <- renderPrint({
      result
    })
  })

  # Pruebas de normalidad
  observeEvent(input$test_normality, {
    req(values$data, values$selected_vars_normality)
    values$normality_results <- normality_test_SW(values$data, values$selected_vars_normality)
    showNotification("Pruebas de normalidad completadas!", duration = 3)
  })

  # Output: resultados de normalidad
  output$normality_results <- DT::renderDataTable({
    req(values$normality_results)
    DT::datatable(values$normality_results, options = list(scrollX = TRUE))
  })

  # Gráficos
  output$boxplots <- renderPlot({
    req(values$data, values$selected_vars_plots)
    if (length(values$selected_vars_plots) > 0) {
      grafico_boxplots(values$data, values$selected_vars_plots)
    }
  })

  output$correlation_plot <- renderPlotly({
    req(values$data, values$selected_vars_plots)
    if (length(values$selected_vars_plots) > 1) {
      numeric_data <- values$data[values$selected_vars_plots] %>% select_if(is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")

      p <- plot_ly(z = cor_matrix, type = "heatmap",
                   x = colnames(cor_matrix), y = rownames(cor_matrix),
                   colorscale = "RdBu", zmid = 0)
      p
    }
  })

  # Tablas de resultados - botón para calcular
  observeEvent(input$calculate_tables, {
    req(values$data, values$selected_vars_tables)

    if (length(values$selected_vars_tables) >= 2) {
      start_col <- values$selected_vars_tables[1]
      end_col <- values$selected_vars_tables[length(values$selected_vars_tables)]

      # Calcular descriptivos
      values$descriptive_results <- calculate_descriptives(values$data, start_col, end_col)

      # Calcular correlaciones
      values$correlation_results <- calcular_correlaciones(values$data, start_col, end_col)

      # Calcular comparaciones por grupo si se seleccionó una variable de agrupación
      if (!is.null(input$group_var) && input$group_var != "") {
        values$comparative_results <- Calcule_Comparative(
          values$data,
          values$selected_vars_tables,
          group_var = input$group_var,
          Robust = input$robust_analysis
        )
      }

      # Calcular fiabilidad y Tabla 1 si hay archivo de texto
      if (!is.null(values$text_data) && length(values$selected_vars_tables) >= 2) {
        extracted <- extract_items(values$text_data)
        values$fiabilidad_results <- calcula_omega_all(extracted, values$data)

        # Crear Tabla 1 combinando descriptivos y fiabilidad
        if (!is.null(values$descriptive_results) && !is.null(values$fiabilidad_results)) {
          values$tabla1_results <- inner_join(values$descriptive_results, values$fiabilidad_results, by = "Variables")
        }
      }
    }
  })

  output$descriptive_table <- DT::renderDataTable({
    req(values$descriptive_results)
    DT::datatable(values$descriptive_results, options = list(scrollX = TRUE))
  })

  output$correlation_table <- DT::renderDataTable({
    req(values$correlation_results)
    if (!is.null(values$correlation_results$correlation)) {
      cor_matrix <- values$correlation_results$correlation

      # Asegurar que sea una matriz numérica
      if (is.matrix(cor_matrix) || is.data.frame(cor_matrix)) {
        cor_matrix <- as.matrix(cor_matrix)
        cor_matrix <- round(cor_matrix, 3)

        # Convertir a data.frame con nombres apropiados
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
    DT::datatable(values$comparative_results, options = list(scrollX = TRUE))
  })

  output$tabla1 <- DT::renderDataTable({
    req(values$tabla1_results)
    DT::datatable(values$tabla1_results, options = list(scrollX = TRUE))
  })

  # Descargas de tablas
  output$download_normality_table <- downloadHandler(
    filename = function() {
      paste("Normalidad_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$normality_results)
      write.xlsx(values$normality_results, file)
    }
  )

  output$download_descriptive <- downloadHandler(
    filename = function() {
      paste("Tabla1_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$tabla1_results)
      write.xlsx(values$tabla1_results, file)
    }
  )

  output$download_correlation_matrix <- downloadHandler(
    filename = function() {
      paste("Tabla2_Correlaciones_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$correlation_results)
      if (!is.null(values$correlation_results$correlation)) {
        cor_matrix <- values$correlation_results$correlation
        # Convertir a data.frame para Excel
        cor_df <- data.frame(
          Variable = rownames(cor_matrix),
          cor_matrix,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        write.xlsx(cor_df, file, rowNames = FALSE)
      }
    }
  )

  output$download_comparative <- downloadHandler(
    filename = function() {
      paste("Tabla3_Comparaciones_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$comparative_results)
      write.xlsx(values$comparative_results, file)
    }
  )

  output$download_tabla1 <- downloadHandler(
    filename = function() {
      paste("Tabla1_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$tabla1_results)
      write.xlsx(values$tabla1_results, file)
    }
  )

  # Descargas de gráficos
  output$download_boxplot <- downloadHandler(
    filename = function() {
      paste("Boxplots_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(values$data, values$selected_vars_plots)
      if (length(values$selected_vars_plots) > 0) {
        p <- grafico_boxplots(values$data, values$selected_vars_plots)
        ggsave(filename = file, plot = p, width = 10, height = 6, dpi = 300, device = "jpeg")
      }
    }
  )

  output$download_correlation <- downloadHandler(
    filename = function() {
      paste("Correlacion_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      req(values$data, values$selected_vars_plots)
      if (length(values$selected_vars_plots) > 1) {
        numeric_data <- values$data[values$selected_vars_plots] %>% select_if(is.numeric)
        cor_matrix <- cor(numeric_data, use = "complete.obs")

        # Crear gráfico de correlación con ggplot para poder guardarlo
        cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
        cor_df$value <- as.vector(cor_matrix)

        p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Matriz de Correlaciones")

        ggsave(filename = file, plot = p, width = 10, height = 8, dpi = 300, device = "jpeg")
      }
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
