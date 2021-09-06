# Título: "TFM - Mapa del Precio de la Vivienda a nivel internacional"
# Autores: "Daniel Corral Ruiz, Antonio Pascual Hernández,  Diego Senso González"
# Datos: Organización para la Cooperación y el Desarrollo Económicos (OCDE)
# Fecha: "10/09/2021"
# Asignatura: Trabajo de Fin de Máster

# Carga de librerías
library(readxl)
library(rsconnect)
library(tidyverse)
library(shiny)
library(ggplot2)
library(tidyr)
library(Hmisc)
library(dygraphs)
library(sf)
library(geojsonio)
library(shinyHeatmaply)
library(leaflet)
library(viridis)
library(RColorBrewer)
library(shinydashboard)
library(rgdal)
library(memisc)


# Carga del dataset y tratamiento de variables
covid <- read.csv("data/paises.csv" , sep = ";")
colnames(covid)[1] <- "country"

# Elección de variables para el análisis clúster
vars <- setdiff(names(covid[,c(3,4,5,6,7)]), "country")

# Carga del archivo geo.json con el que se construirá el mapa
geo <- geojson_read("data/countries.geo.json", what = "sp")

# Fijamos valor inicial de cero para las variables
geo@data$NOMINAL <- 0
geo@data$PRECIO_INGRESOS <- 0
geo@data$PRECIO_ALQUILER <- 0
geo@data$REAL <- 0
geo@data$ALQUILER <- 0

# Se genera una función auxiliar
'%!in%' <- function(x,y)!('%in%'(x,y))

# Tratamiento para que el geo.json reconozca los países por la columna "code" de nuestro dataset
for (i in 1:nrow(geo@data)) {
    id = geo@data$id[i]
    if (id %!in% covid$code) next()
    geo@data$NOMINAL[i] <- covid$NOMINAL[covid$code == id]
    geo@data$PRECIO_INGRESOS[i] <- covid$PRECIO_INGRESOS[covid$code == id]
    geo@data$PRECIO_ALQUILER[i] <- covid$PRECIO_ALQUILER[covid$code == id]
    geo@data$REAL[i] <- covid$REAL[covid$code == id]
    geo@data$ALQUILER[i] <- covid$ALQUILER[covid$code == id]

}

# Uso de función para dar color al mapa
colores <- colorBin(
    domain = geo@data$NOMINAL, 
    bins = 20, 
    palette = heat.colors(20),   
    pretty = T 
)

colors <- colores(geo@data$NOMINAL)


#UI
ui <- dashboardPage(skin = "yellow",
    
    # Título
    dashboardHeader(title = "Vivienda Mundial"),
    
    # Barra lateral
    dashboardSidebar(
      
      sidebarMenu(
        # Creación de las seis pestañas diferentes de la app
        menuItem("Inicio", tabName = "inicio", icon = icon("home")),
        menuItem("Mapa del Mundo", tabName = "map", icon = icon("globe-europe")),
        # Introducción de un desplegable con las opciones del mapa
        menuItem("Opciones del Mapa", tabName = "options", icon = icon("filter"),
                 selectInput("variable", "Escoja una variable para visualizar en el mapa:", 
                             c("Precio Nominal" = "NOMINAL",
                               "Precio en relación a la renta" = "PRECIO_INGRESOS", 
                               "Precio en relación al alquiler" = "PRECIO_ALQUILER",
                               "Precio Real" = "REAL",
                               "Precios del alquiler" = "ALQUILER")
                 ),
                 selectInput("variable2", "Escoja una variable para ver la correlación con la primera:", 
                             c("Precio Nominal" = "NOMINAL",
                               "Precio en relación a la renta" = "PRECIO_INGRESOS", 
                               "Precio en relación al alquiler" = "PRECIO_ALQUILER",
                               "Precio Real" = "REAL",
                               "Precios del alquiler" = "ALQUILER"),
                             selected = "PRECIO_INGRESOS"
                 ),

                 radioButtons("palette", label = h3("Escoja la paleta de colores para el mapa:"),
                              choices = list("Rojo" = "Red", "Verde" = "Green", "Azul" = "Blue", "Naranja" = "Orange"), 
                              selected = "Red")
        ),
        
        # Pestañas restantes
        menuItem("Análisis Clúster", tabName = "cluster", icon = icon("object-group")),
        menuItem("Higher or Lower", tabName = "juego", icon = icon("gamepad")))),
      
        
    
    # Main Body
    dashboardBody(
      # Introducción del botón musical
      h4("Si lo desea, pulsando el 'Play' puede añadir música mientras visualiza los datos:"),
      HTML('<iframe src="https://drive.google.com/file/d/1CET9p1_GeZUtGiymnIPpxrqkCr5EEJAQ/preview" width="440" height="60"></iframe>'),
      
            
        tabItems(
          #  Pestaña que aparece al abrir la app. Muestra un texto de bienvenida y una imagen. 
          tabItem(
            tabName = "inicio",
            h1("Bienvenido a la aplicación"),
            h3("Seleccione una de las pestañas en el panel de la izquierda para comenzar a visualizar los datos. Para un mejor uso, recomendamos seleccionar la opción superior 'Open in Browser'."),
            tags$img(src = "https://media.discordapp.net/attachments/777928242086150157/883012860278632480/7bae2d0b-a8ad-469a-8b2d-923bd46bc7e3_alta-libre-aspect-ratio_default_0.png?width=1133&height=663",
                     width = "900",
                     height = "550")
            ),
          
          # Pestaña con el mapa. Cuenta con un título, explicación de la variable elegida, el mapa interactivo, un selector de países, cálculos de correlación, dos histogramas y una tabla de datos.
          tabItem(
            tabName = "map",
            h1("Precios de la Vivienda en el Mundo"),
            fluidRow(textOutput("diccionario"),
                      h3("Mapa del Mundo"),
                      leafletOutput("map"),
                      h3("Histogramas y correlación para las variables y países escogidos."),
                      selectInput("value", label = h3("Escoja los países para visualizar en los histogramas y la tabla inferior:"), 
                                 choices = covid[,1], 
                                 multiple = TRUE,
                                 selected = "Spain"),
                     
                      textOutput("Correlacion"),
                     
                      box(plotOutput(outputId = "hist1")),
                      box(plotOutput(outputId = "hist2")),
                      h3("Tabla con todos los indicadores de los países escogidos:"),
                      dataTableOutput("tabla"),
                      fluidRow(verbatimTextOutput("map_shape_click")))
          ),
          
          # Pestaña de Análisis clúster. Incluye la elección de dos variables y número de grupos, el gráfico y una tabla de los países de cada grupo.
          tabItem(
            tabName = "cluster",
            h1("Análisis Clúster"),
            h4("Este Análisis Clúster sirve para agrupar los países en función de dos variables. Cada grupo estará formado por los países con datos más similares. Para realizar la agrupación de países por clústers, seleccione las dos variables por las cuales agrupar y el número de grupos que desea. En el gráfico inferior, cada punto representará un país y cada color será un grupo diferente."),
            selectInput('xcol', 'Escoja la primera variable:', vars, selected = "NOMINAL"),
            selectInput('ycol', 'Escoja la segunda variable:', vars, selected = "PRECIO_INGRESOS"),
            numericInput('clusters', 'Escoja el número de grupos a realizar:', 3, min = 1, max = 10),
            plotOutput("plotcluster"),
            h3("En la tabla siguiente, puede observar a qué grupo pertenece cada país en función de la configuración escogida:"),
            dataTableOutput("tablacluster")
          ),
          
          # Pestaña con un minijuego donde se eligen dos países y muestra cuál cuenta con un indicador más elevado en cada variable, más una tabla de los países.
          tabItem(
            tabName = "juego",
            h1("Higher or Lower: Edición Precios de la Vivienda en el Mundo"),
            h3("Seleccione dos países y podrá observar si los índices de precios del primero son mayores, menores o iguales a los del segundo. Todos los indicadores tienen base en el año 2015, por lo que podrá comprobar en qué países han aumentado más o menos los diferentes indicadores en los últimos años."),
            selectInput("juegopais", label = h3("Escoja el primer país:"), 
                        choices = covid[,1], 
                        multiple = FALSE,
                        selected = "Spain"),
            selectInput("juegopais2", label = h3("Escoja el segundo país:"), 
                        choices = covid[,1], 
                        multiple = FALSE,
                        selected = "Portugal"),
            textOutput("juegocasos"),
            textOutput("juegomuertes"),
            textOutput("juegopoblacion"),
            textOutput("juegomortalidad"),
            textOutput("juegoincidencia_100.000"),
            textOutput("juegomuertes_100.000"),
            
            h3("A continuación, se ofrecen los índices de los dos países escogidos de forma numérica:"),
            dataTableOutput("tablajuego1"),
            dataTableOutput("tablajuego2")
          )
        )
    )
)

#SERVER
server <- function(input, output){
  
  # Reacción del mapa
  value <- reactiveValues(click_shape = NULL)
  
  observeEvent(input$reset, { 
    value$click_shape <- NULL
  })
  
  observeEvent(input$map_shape_click, {
    value$click_shape <- input$map_shape_click$id
  })
  
  # Texto explicativo de la variable seleccionada
    output$diccionario <- renderText({   
    
      variable1 <- input$variable
    
        if (variable1 == "NOMINAL") {print("El *Precio Nominal* es el valor de la vivienda llevando incluido el efecto de la inflación (Índice con base en el año 2015).")} 
        else if (variable1 == "PRECIO_INGRESOS") {print("La *Relación Precio-Renta* es el índice de precios de la vivienda nominal dividido por la renta disponible nominal per cápita y puede considerarse como una medida de asequibilidad (Índice con base en el año 2015).")} 
        else if (variable1 == "PRECIO_ALQUILER") {print("La *Relación Precio-Alquiler* es el índice de precios nominales de la vivienda dividido por el índice de precios del alquiler de la vivienda y puede considerarse como una medida de la rentabilidad de la propiedad de la vivienda (Índice con base en el año 2015).")} 
        else if (variable1 == "REAL") {print("El *Precio Real* es el valor de la vivienda descontado el efecto de la inflación o deflación (Índice con base en el año 2015).")} 
        else if (variable1 == "ALQUILER") {print("El *Alquiler* es el índice de precios de los alquileres (Índice con base en el año 2015).")}
        else {print("No variable")}
  })
    
    # Función de colores
    colores <- reactive({
        colorBin(
            if(input$palette == "Blue"){palette = brewer.pal(3, "Blues")}
            else if(input$palette == "Red"){palette = brewer.pal(3, "Reds")}
            else if(input$palette == "Orange"){palette = rev(magma(587))}
            else palette = rev(viridis(587)),
            domain = geo@data[,input$variable],
            bins = 10,
            pretty = T
        )
    })
    # Representacion del mapa
    observe({
        colors <- colores()
        map <- leaflet(geo) %>%
            addTiles() %>%
            addPolygons(
                stroke = F,
                smoothFactor = 0.1,
                fillOpacity = .95,
                color = ~colors(geo@data[,input$variable]),
                highlight = highlightOptions(
                  weight = 5,
                  color = "#663",
                  fillOpacity = 0.2,
                  bringToFront = TRUE),
                  label = paste(sep = ": ", geo@data$name, geo@data[,input$variable]),
                  layerId =  paste(sep = ": ", geo@data$name, geo@data[,input$variable]),
                  labelOptions =  labelOptions(
                  style = list("font-weight" = "bold-italic", padding = "5px 8px"),
                  textsize = "15px",
                  direction = "auto")
                
            ) %>%
          # Leyenda del mapa en función de la variable escogida
          addLegend("topright", pal = colors, values = input$variable,
                    title = "Leyenda variable seleccionada" ,
                    opacity = 1
          ) %>%
            setView(lng = 40,
                    lat = 40,
                    zoom = 1.25)
            
    output$map <- renderLeaflet(map)
    
    })
    
    # Histograma de la variable 1
    output$hist1 <- renderPlot({
      var1 <- isolate(input$variable)
      hist(dataset()[[var1]], main = paste("Histograma de la variable:",toupper(input$variable)), 
           xlab = paste("Valores de la variable:",toupper(input$variable)), 
           ylab = "Frecuencia", 
           col = input$palette)
      
    })
    # Histograma de la variable 2
    output$hist2 <- renderPlot({
      var2 <- isolate(input$variable2)
      hist(dataset()[[var2]], main = paste("Histograma de la variable:",toupper(input$variable2)), 
           xlab = paste("Valores de la variable:",toupper(input$variable2)), 
           ylab = "Frecuencia", 
           col = input$palette)
      
    })
    
    # Representación de la correlación
    output$Correlacion <- renderText({
      
      variable1 <- isolate(input$variable)
      variable2 <- isolate(input$variable2)
      
      print(paste0("La correlación entre las variables ", capitalize(variable1), " y ", capitalize(variable2), " para los países seleccionados es: ", 
                   round(cor(dataset()[[variable1]], dataset()[[variable2]]),2)))
    })
    
    # Creación de un reactive para introducir en la tabla los datos de los países seleccionados
    dataset <- reactive({ 
      print(input$value)
      data.frame(covid[covid$country %in% input$value,])
    })
    
    # Mostramos la tabla
    output$tabla <- renderDataTable({dataset()})
    
   
  # Clusters
    
     # Selección de las variables para realizar el análisis cluster
    selectedData <- reactive({
      covid[, c(input$xcol, input$ycol)]
    })
    
    # Agrupación por K-medias
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    
    clusterdata <- reactive({data.frame(clusters())})
    
    # Representación del análisis cluster en gráfico y con colores
    output$plotcluster <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)})
    
    # Creación de dataframe con cada país y el clúster al que pertenece
    cluster_country <- reactive({
      data.frame(País = covid$country, 
                 Grupo = clusters()$cluster)
    })
    
    # Representación de la tabla con cada país y su número de clúster
    output$tablacluster <- renderDataTable({cluster_country()})
    
  # Higher or lower
    
    # Creación de dataframe con los datos de los dos paises elegidos
    datasetjuego <- reactive({ 
      data.frame(covid[covid$country %in% input$juegopais,])
    })
    
    datasetjuego2 <- reactive({ 
      data.frame(covid[covid$country %in% input$juegopais2,])
    })
    
    # Creación del output de las comparaciones entre los paises
    output$juegocasos <- renderText({
      
      if (datasetjuego()$NOMINAL > datasetjuego2()$NOMINAL) {print("Precio Nominal: HIGHER")} 
      else if (datasetjuego()$NOMINAL < datasetjuego2()$NOMINAL) {print("Precio Nominal: LOWER")}
      else {print("Precio Nominal: EQUAL")}
    })
    
    output$juegomuertes <- renderText({
      
      if (datasetjuego()$PRECIO_INGRESOS > datasetjuego2()$PRECIO_INGRESOS) {print("Precio en relación a la renta: HIGHER")} 
      else if (datasetjuego()$PRECIO_INGRESOS < datasetjuego2()$PRECIO_INGRESOS) {print("Precio en relación a la renta: LOWER")}
      else {print("Precio en relación a la renta: EQUAL")}
    })
    
    output$juegopoblacion <- renderText({
      
      if (datasetjuego()$PRECIO_ALQUILER > datasetjuego2()$PRECIO_ALQUILER) {print("Precio en relación al alquiler: HIGHER")} 
      else if (datasetjuego()$PRECIO_ALQUILER < datasetjuego2()$PRECIO_ALQUILER) {print("Precio en relación al alquiler: LOWER")}
      else {print("Precio en relación al alquiler: EQUAL")}
    })
    
    output$juegomortalidad <- renderText({
    
      if (datasetjuego()$REAL > datasetjuego2()$REAL) {print("Precio Real: HIGHER")} 
      else if (datasetjuego()$REAL < datasetjuego2()$REAL) {print("Precio Real: LOWER")}
      else {print("Precio Real: EQUAL")}
    })
    
    output$juegoincidencia_100.000 <- renderText({ 
      
      if (datasetjuego()$ALQUILER > datasetjuego2()$ALQUILER) {print("Precio del Alquiler: HIGHER")} 
      else if (datasetjuego()$ALQUILER < datasetjuego2()$ALQUILER) {print("Precio del Alquiler: LOWER")}
      else {print("Precio del Alquiler: EQUAL")}
    })
    
    # Creación de una tabla para mostrar los datos de cada país seleccionado
    output$tablajuego1 <- renderDataTable({datasetjuego()})
    output$tablajuego2 <- renderDataTable({datasetjuego2()})
    
}
# run shiny
shinyApp(ui,server)

