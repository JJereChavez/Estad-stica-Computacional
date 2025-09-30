#
# Aplicación Shiny: Análisis de Correspondencias Múltiples (ACM)
# Autor: Tu nombre
# Fecha: 2025
#

library(shiny)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(DT)
library(shinythemes)

# Define UI
ui <- navbarPage(
  title = "Análisis de Correspondencias Múltiples",
  theme = shinytheme("flatly"),
  
  # Pestaña 1: Introducción
  tabPanel("📚 Introducción",
           fluidRow(
             column(10, offset = 1,
                    br(),
                    h2("¿Qué es el Análisis de Correspondencias Múltiples?"),
                    br(),
                    p(style = "font-size: 16px; text-align: justify;",
                      "El Análisis de Correspondencias Múltiples (ACM) es una técnica estadística 
                          exploratoria diseñada para analizar tablas de datos categóricos. Es una extensión 
                          del Análisis de Correspondencias Simple (ACS) que permite estudiar las relaciones 
                          entre más de dos variables cualitativas simultáneamente."),
                    
                    h3("🎯 Objetivos del ACM:"),
                    tags$ul(
                      tags$li("Reducir la dimensionalidad de datos categóricos"),
                      tags$li("Visualizar las relaciones entre categorías de diferentes variables"),
                      tags$li("Identificar perfiles de individuos similares"),
                      tags$li("Detectar asociaciones entre modalidades de variables")
                    ),
                    
                    h3("📊 ¿Cuándo usar ACM?"),
                    tags$ul(
                      tags$li("Cuando todas las variables son cualitativas (categóricas)"),
                      tags$li("Para explorar encuestas con preguntas categóricas"),
                      tags$li("Para segmentación de mercado o perfiles de clientes"),
                      tags$li("En estudios sociológicos y de opinión pública")
                    ),
                    
                    h3("🔍 Interpretación:"),
                    tags$ul(
                      tags$li(strong("Cercanía:"), " Categorías cercanas en el gráfico tienden a aparecer juntas"),
                      tags$li(strong("Ejes:"), " Las dimensiones representan factores latentes que explican la variabilidad"),
                      tags$li(strong("Contribución:"), " Indica qué variables/categorías definen mejor cada dimensión")
                    ),
                    br(),
                    div(style = "background-color: #e8f4f8; padding: 15px; border-radius: 5px;",
                        h4("💡 Consejo:"),
                        p("Comienza cargando tus datos en la pestaña 'Datos' o utiliza el dataset de ejemplo 
                              'tea' para familiarizarte con la aplicación.")
                    )
             )
           )
  ),
  
  # Pestaña 2: Datos
  tabPanel("📁 Datos",
           sidebarLayout(
             sidebarPanel(
               h4("Cargar datos"),
               radioButtons("data_source", "Fuente de datos:",
                            choices = c("Dataset de ejemplo (tea)" = "example",
                                        "Cargar archivo CSV" = "upload")),
               
               conditionalPanel(
                 condition = "input.data_source == 'upload'",
                 fileInput("file", "Selecciona archivo CSV:",
                           accept = c("text/csv", ".csv")),
                 checkboxInput("header", "¿Incluye encabezados?", TRUE),
                 radioButtons("sep", "Separador:",
                              choices = c("Coma" = ",", "Punto y coma" = ";", "Tabulador" = "\t"))
               ),
               
               conditionalPanel(
                 condition = "input.data_source == 'example'",
                 div(style = "background-color: #fff3cd; padding: 10px; border-radius: 5px;",
                     p(strong("Dataset Tea:"), "Contiene información sobre hábitos de consumo de té 
                               en 300 individuos con 36 variables categóricas.")
                 )
               ),
               br(),
               actionButton("load_data", "Cargar datos", class = "btn-primary btn-block")
             ),
             
             mainPanel(
               h3("Vista previa de los datos"),
               DTOutput("data_preview"),
               br(),
               verbatimTextOutput("data_summary")
             )
           )
  ),
  
  # Pestaña 3: Configuración ACM
  tabPanel("⚙️ Configuración",
           sidebarLayout(
             sidebarPanel(
               h4("Configuración del ACM"),
               uiOutput("var_selection"),
               numericInput("ncp", "Número de dimensiones:",
                            value = 5, min = 2, max = 10),
               br(),
               actionButton("run_mca", "Ejecutar ACM", class = "btn-success btn-block"),
               br(),
               uiOutput("mca_status")
             ),
             
             mainPanel(
               h3("Valores propios y varianza explicada"),
               plotOutput("scree_plot", height = "400px"),
               br(),
               h4("Tabla de valores propios"),
               DTOutput("eigenvalues_table")
             )
           )
  ),
  
  # Pestaña 4: Visualizaciones
  tabPanel("📊 Visualizaciones",
           fluidRow(
             column(3,
                    wellPanel(
                      h4("Opciones de gráfico"),
                      selectInput("plot_type", "Tipo de gráfico:",
                                  choices = c("Individuos" = "ind",
                                              "Variables (categorías)" = "var",
                                              "Biplot" = "biplot")),
                      selectInput("dim_x", "Dimensión X:", choices = 1:5, selected = 1),
                      selectInput("dim_y", "Dimensión Y:", choices = 1:5, selected = 2),
                      sliderInput("point_size", "Tamaño de puntos:", 
                                  min = 1, max = 5, value = 2),
                      checkboxInput("show_labels", "Mostrar etiquetas", TRUE),
                      conditionalPanel(
                        condition = "input.plot_type == 'ind'",
                        uiOutput("color_var_ui")
                      )
                    )
             ),
             column(9,
                    plotOutput("mca_plot", height = "600px", width = "100%")
             )
           )
  ),
  
  # Pestaña 5: Resultados
  tabPanel("📈 Resultados",
           tabsetPanel(
             tabPanel("Contribuciones de variables",
                      br(),
                      h4("Contribuciones a las dimensiones (%)"),
                      p("Las variables con mayor contribución son las que mejor definen cada dimensión."),
                      selectInput("contrib_dim", "Seleccionar dimensión:", 
                                  choices = paste("Dim", 1:5)),
                      plotOutput("contrib_plot", height = "500px"),
                      br(),
                      DTOutput("contrib_table")
             ),
             
             tabPanel("Calidad de representación",
                      br(),
                      h4("Cos2 - Calidad de representación"),
                      p("Valores cercanos a 1 indican buena representación en el plano factorial."),
                      selectInput("cos2_type", "Mostrar cos2 para:",
                                  choices = c("Variables" = "var", "Individuos" = "ind")),
                      plotOutput("cos2_plot", height = "500px")
             ),
             
             tabPanel("Coordenadas",
                      br(),
                      h4("Coordenadas de las categorías"),
                      DTOutput("coord_table")
             )
           )
  ),
  
  # Pestaña 6: Ayuda
  tabPanel("❓ Ayuda",
           fluidRow(
             column(10, offset = 1,
                    br(),
                    h2("Guía de uso"),
                    br(),
                    h3("1️⃣ Cargar datos"),
                    p("Ve a la pestaña 'Datos' y elige entre usar el dataset de ejemplo o cargar tu propio archivo CSV. 
                          Asegúrate de que tus datos sean categóricos (variables cualitativas)."),
                    
                    h3("2️⃣ Configurar análisis"),
                    p("En la pestaña 'Configuración', selecciona las variables que deseas incluir en el ACM. 
                          Puedes elegir el número de dimensiones a calcular (generalmente 5 es suficiente)."),
                    
                    h3("3️⃣ Interpretar resultados"),
                    tags$ul(
                      tags$li(strong("Scree plot:"), " Muestra la varianza explicada por cada dimensión. 
                                    Busca el 'codo' para determinar cuántas dimensiones retener."),
                      tags$li(strong("Gráfico de individuos:"), " Muestra cómo se distribuyen los casos estudiados."),
                      tags$li(strong("Gráfico de variables:"), " Muestra las relaciones entre categorías. 
                                    Categorías cercanas tienden a aparecer juntas."),
                      tags$li(strong("Biplot:"), " Combina individuos y variables en un solo gráfico.")
                    ),
                    
                    h3("📚 Recursos adicionales"),
                    tags$ul(
                      tags$li(tags$a("FactoMineR Documentation", 
                                     href = "http://factominer.free.fr/", target = "_blank")),
                      tags$li(tags$a("factoextra Package", 
                                     href = "https://rpkgs.datanovia.com/factoextra/", target = "_blank"))
                    ),
                    br()
             )
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  data <- reactiveVal(NULL)
  mca_result <- reactiveVal(NULL)
  
  # Cargar datos
  observeEvent(input$load_data, {
    if (input$data_source == "example") {
      # Cargar el dataset tea directamente
      tryCatch({
        library(FactoMineR)
        data(tea)
        data(tea)
        showNotification("Dataset 'tea' cargado exitosamente", type = "message")
      }, error = function(e) {
        showNotification("Error: Asegúrate de tener instalado FactoMineR", type = "error")
      })
    } else {
      req(input$file)
      tryCatch({
        df <- read.csv(input$file$datapath, 
                       header = input$header,
                       sep = input$sep,
                       stringsAsFactors = TRUE)
        data(df)
        showNotification("Archivo cargado exitosamente", type = "message")
      }, error = function(e) {
        showNotification(paste("Error al cargar archivo:", e$message), type = "error")
      })
    }
  })
  
  # Vista previa de datos
  output$data_preview <- renderDT({
    req(data())
    datatable(head(data(), 100), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Resumen de datos
  output$data_summary <- renderPrint({
    req(data())
    cat("Dimensiones:", nrow(data()), "filas x", ncol(data()), "columnas\n\n")
    cat("Estructura de variables:\n")
    str(data())
  })
  
  # Selección de variables
  output$var_selection <- renderUI({
    req(data())
    # Filtrar solo variables categóricas
    cat_vars <- names(data())[sapply(data(), function(x) is.factor(x) || is.character(x))]
    
    checkboxGroupInput("selected_vars", 
                       "Selecciona variables para el ACM:",
                       choices = cat_vars,
                       selected = cat_vars[1:min(5, length(cat_vars))])
  })
  
  # Ejecutar ACM
  observeEvent(input$run_mca, {
    req(data(), input$selected_vars)
    
    tryCatch({
      # Convertir a factores si es necesario
      df_mca <- data()[, input$selected_vars, drop = FALSE]
      df_mca <- as.data.frame(lapply(df_mca, as.factor))
      
      # Ejecutar MCA
      res_mca <- MCA(df_mca, ncp = input$ncp, graph = FALSE)
      mca_result(res_mca)
      
      showNotification("ACM ejecutado exitosamente", type = "message")
    }, error = function(e) {
      showNotification(paste("Error en ACM:", e$message), type = "error")
    })
  })
  
  # Estado del ACM
  output$mca_status <- renderUI({
    if (!is.null(mca_result())) {
      div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; color: #155724;",
          icon("check-circle"), " ACM ejecutado correctamente"
      )
    } else {
      div(style = "background-color: #f8d7da; padding: 10px; border-radius: 5px; color: #721c24;",
          icon("exclamation-circle"), " ACM no ejecutado aún"
      )
    }
  })
  
  # Scree plot
  output$scree_plot <- renderPlot({
    req(mca_result())
    fviz_screeplot(mca_result(), addlabels = TRUE, ylim = c(0, 50),
                   main = "Varianza explicada por dimensión",
                   xlab = "Dimensiones", ylab = "% Varianza explicada")
  })
  
  # Tabla de valores propios
  output$eigenvalues_table <- renderDT({
    req(mca_result())
    eig <- as.data.frame(mca_result()$eig)
    eig$Dimension <- paste("Dim", 1:nrow(eig))
    eig <- eig[, c(4, 1, 2, 3)]
    colnames(eig) <- c("Dimensión", "Valor propio", "% Varianza", "% Acumulado")
    datatable(eig, options = list(pageLength = 10)) %>%
      formatRound(2:4, 2)
  })
  
  # UI para colorear por variable
  output$color_var_ui <- renderUI({
    req(data(), input$selected_vars)
    selectInput("color_var", "Colorear por variable:",
                choices = c("Ninguna" = "none", input$selected_vars),
                selected = "none")
  })
  
  # Gráficos principales
  output$mca_plot <- renderPlot({
    req(mca_result())
    
    axes <- c(as.numeric(input$dim_x), as.numeric(input$dim_y))
    
    if (input$plot_type == "ind") {
      if (!is.null(input$color_var) && input$color_var != "none") {
        habillage_var <- which(input$selected_vars == input$color_var)
        fviz_mca_ind(mca_result(), axes = axes, 
                     geom.ind = "point",
                     pointsize = input$point_size,
                     habillage = habillage_var,
                     addEllipses = TRUE,
                     repel = input$show_labels,
                     title = "Gráfico de individuos")
      } else {
        fviz_mca_ind(mca_result(), axes = axes,
                     geom.ind = "point",
                     pointsize = input$point_size,
                     repel = input$show_labels,
                     title = "Gráfico de individuos")
      }
    } else if (input$plot_type == "var") {
      fviz_mca_var(mca_result(), axes = axes,
                   repel = input$show_labels,
                   labelsize = 3,
                   title = "Gráfico de variables (categorías)")
    } else {
      fviz_mca_biplot(mca_result(), axes = axes,
                      repel = input$show_labels,
                      title = "Biplot (individuos y variables)")
    }
  })
  
  # Gráfico de contribuciones
  output$contrib_plot <- renderPlot({
    req(mca_result())
    dim_num <- as.numeric(gsub("Dim ", "", input$contrib_dim))
    fviz_contrib(mca_result(), choice = "var", axes = dim_num,
                 top = 20,
                 title = paste("Contribución de variables a", input$contrib_dim))
  })
  
  # Tabla de contribuciones
  output$contrib_table <- renderDT({
    req(mca_result())
    dim_num <- as.numeric(gsub("Dim ", "", input$contrib_dim))
    contrib <- as.data.frame(mca_result()$var$contrib[, dim_num, drop = FALSE])
    contrib$Variable <- rownames(contrib)
    contrib <- contrib[order(-contrib[, 1]), ]
    contrib <- contrib[, c(2, 1)]
    colnames(contrib) <- c("Variable/Categoría", "Contribución (%)")
    datatable(contrib, options = list(pageLength = 15)) %>%
      formatRound(2, 2)
  })
  
  # Gráfico de cos2
  output$cos2_plot <- renderPlot({
    req(mca_result())
    fviz_cos2(mca_result(), choice = input$cos2_type, axes = 1:2,
              top = 20,
              title = paste("Calidad de representación -", 
                            ifelse(input$cos2_type == "var", "Variables", "Individuos")))
  })
  
  # Tabla de coordenadas
  output$coord_table <- renderDT({
    req(mca_result())
    coord <- as.data.frame(mca_result()$var$coord)
    coord$Categoria <- rownames(coord)
    coord <- coord[, c(ncol(coord), 1:(ncol(coord)-1))]
    datatable(coord, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatRound(2:ncol(coord), 3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)