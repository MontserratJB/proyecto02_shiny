# Paquetes
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(shiny)
library(shinydashboard)

# Datos

# Lectura de una capa vectorial (GeoJSON) de distritos de Montes de Oca
distritos <-
    st_read(
        "https://raw.githubusercontent.com/MontserratJB/Proyecto-2021/master/Distritos.geojson",
        quiet = TRUE
    )

# Lectura de un archivo CSV con datos de Presupuestos participativos en Montes de Oca
presupuesto <-
    st_read(
        "/vsicurl/https://raw.githubusercontent.com/MontserratJB/tarea03-shinyapp/master/Pres_part.csv",
        options = c(
            "X_POSSIBLE_NAMES=decimalLongitude",
            "Y_POSSIBLE_NAMES=decimalLatitude"
        ),
        quiet = TRUE
    )

# Asignación de un CRS al objeto felidae
st_crs(presupuesto) <- 4326

#Lectura de vias primarias
viaprimaria <-
    st_read(
        "https://raw.githubusercontent.com/MontserratJB/proyecto02_shiny/master/viasprimarias.geojson",
        quiet = TRUE
    )

# Asignación de un CRS al objeto viaprimaria
st_crs(viaprimaria) <- 4326

# Lectura de una capa raster de altitud
altitud <-
    rast(
        "/vsicurl/https://raw.githubusercontent.com/tpb728O-programaciongeoespacialr/2021ii/master/datos/worldclim/altitud.tif"
    )


# Lista ordenada de distritos + "Todas"
lista_distritos <- unique(presupuesto$Distrito)
lista_distritos <- sort(lista_distritos)
lista_distritos <- c("Todas", lista_distritos)

# Lista ordenada de numero de proyecto por distrito + "Todas"
lista_presupuestos <- unique(presupuesto$Presupuesto)
lista_presupuestos <- sort(lista_presupuestos)
lista_presupuestos <- c("Todas", lista_presupuestos)


# Componentes de la aplicación Shiny
# Definición del objeto ui
ui <-
    dashboardPage(
        dashboardHeader(title = "Presupuestos participativos en el cantón de Montes de Oca"),
        dashboardSidebar(sidebarMenu(
            menuItem(
                text = "Filtros",
                selectInput(
                    inputId = "distrito",
                    label = "Distrito",
                    choices = lista_distritos,
                    selected = "Todas"
                ),
                selectInput(
                    inputId = "id_presupuesto",
                    label = "Presupuesto",
                    choices = lista_presupuestos,
                    selected = "Todas"
                ),
                startExpanded = TRUE
            ) 
        )),
        dashboardBody(fluidRow(
            box(
                title = "Mapa de Presupuestos Participativos",
                leafletOutput(outputId = "mapa"),
                width = 6
            ),
            box(
                title = "Zona de influencia de Proyectos",
                plotOutput(outputId = "mapa02"),
                width = 6
            ),
            box(
                title = "Grafico por Distrito",
                plotlyOutput(outputId = "grafico_distritos"),
                width = 6
            ),
            fluidRow(
                box(
                    title = "Tabla de proyectos",
                    DTOutput(outputId = "tabla"),
                    width = 6
                )
            ))
        ))

# Definición de la función server
server <- function(input, output, session) {
    filtrarRegistros <- reactive({
        # Remoción de geometrías y selección de columnas
        presupuesto_filtrado <-
            presupuesto %>%
            dplyr::select(Distrito, Presupuesto, Inversion)
        
        # Filtrado de felidae por especie
        if (input$distrito != "Todas") {
            presupuesto_filtrado <-
                presupuesto_filtrado %>%
                filter(Distrito == input$distrito)
        }
        # Filtrado de felidae por provincia
        if (input$id_presupuesto != "Todas") {
            presupuesto_filtrado <-
                presupuesto_filtrado %>%
                filter(Presupuesto == input$id_presupuesto)
        }
        
        return(presupuesto_filtrado)
    })
    
    output$mapa <- renderLeaflet({
        registros <-
            filtrarRegistros()
        
        # Conversión del objeto altitud a la clase RasterLayer
        altitud_rl <- raster::raster(altitud)
        
        # Mapa Leaflet con capas de distritos y registros de presupuestos participativos en Montes de Oca
        leaflet() %>%
            setView(lng = -84.01705,
                    lat = 9.940166,
                    zoom = 13) %>%
            addTiles() %>%
            leafem::addMouseCoordinates() %>%
            addScaleBar() %>%
            addMeasurePathToolbar(
                options = measurePathOptions(imperial = FALSE,
                                             minPixelDistance = 100,
                                             showDistances = TRUE)) %>%
            addSearchOSM( options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
            addProviderTiles(
                providers$Esri.WorldImagery, group = "ESRI") %>%
            addRasterImage(altitud_rl,
                           opacity = 0.6) %>%
            addProviderTiles(
                providers$Stamen.Toner, group = "Toner") %>%
            addPolygons(
                data = distritos,
                color = "red",
                fillColor = "transparent",
                stroke = TRUE,
                weight = 2.0,
                group = "Distritos",
            ) %>%
            addCircleMarkers(
                data = registros,
                stroke = TRUE,
                radius = 4,
                fillColor = 'red',
                fillOpacity = 1,
                label = paste0(
                    registros$Inversion),
                group = "Proyectos",
            ) %>%
            addLayersControl(
                baseGroups = c("Toner","ESRI","Altitud"),
                overlayGroups = c("Distritos","Proyectos"),
                options = layersControlOptions(collapsed = T)
            )
    })
    
    
    output$tabla <- renderDT({
        registros <- filtrarRegistros()
        
        registros %>%
            st_drop_geometry() %>%
            datatable()
    })
    

    # Filtros con presupuesto definido
    output$mapa02 <- renderPlot({
        registros <-
            filtrarRegistros()
        
        # Capas en CRTM05
        viaprimaria02 <-
            st_transform(viaprimaria,5367)
        registros <-
            st_transform(registros,5367)
        distritos02 <-
            st_transform(distritos,5367)
        
        # Proyectos en área de influencia de red primaria
        buffer_via <-
            st_buffer(viaprimaria02, 200)
        
        presupuesto_buffer_via <-
            st_intersection(registros, buffer_via) 
        
        plot(
            st_union(buffer_via),
            main = "Proyectos a 200m de vías Municipales Primarias
            ",
            axes = FALSE,
            graticule = TRUE,
            col_graticule = adjustcolor("green",0),
            tcl = -0.5
        )
        axis(1)
        axis(2)
        
        
        plot(viaprimaria02$geometry,
             col = "blue",
             add = TRUE)
        
        plot(
            presupuesto_buffer_via,
            pch = 16,
            col = "red",
            add = TRUE,
            cex = 02
        )
        plot(
            distritos02$geometry,
            col = adjustcolor("green",0),
            add = TRUE
        )
        
    })
    
    output$grafico_distritos <- renderPlotly({
        registros <- filtrarRegistros()
        
        registros %>%
            st_drop_geometry() %>%
            group_by(Distrito) %>%
            summarize(suma_proyectos = n()) %>%
            plot_ly(x = ~ Distrito,
                    y = ~ suma_proyectos,
                    type="bar", mode="markers", fill = "tozeroy", fillcolor = "green") %>%
            layout(title = "Proyectos por Distrito",
                   xaxis = list(title = "Distritos"),
                   yaxis = list(title = "Número de proyectos")) %>%
            config(locale = "es")
        
    })
}

# Llamado a la función shinyApp()
shinyApp(ui, server)

