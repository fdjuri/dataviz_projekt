library(dplyr)
library(plotly)

library(shiny)
library(leaflet)
library(RColorBrewer)

library(readxl)
library(scales)

library(shinydashboard)

ui <-
  dashboardPage(
    skin = "red",
    header = dashboardHeader(title = "Statistika putnika"),
    sidebar = dashboardSidebar(
      p("Dostupan je period 14. 1. 2019. - 28. 1. 2019."),
      sliderInput(
        inputId = "date",
        label = "Odabrani datum",
        min = as.Date("14012019", format = "%d%m%Y", tz = "UTC"),
        max = as.Date("27012019", format = "%d%m%Y", tz = "UTC"),
        value = as.Date("14012019", format = "%d%m%Y", tz = "UTC"),
        step = 1,
        animate = animationOptions(interval = 1000,
                                   loop = TRUE)
      )
    ),
    body = dashboardBody(fluidRow(
      box(
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        title = "Broj ulazaka po satu",
        plotlyOutput(outputId = "hourDist")
        #plotOutput("hourDist")
      ),
    ),
    fluidRow(
      box(
        width = 12,
        status = "success",
        collapsible = TRUE,
        title = "Karta stanica",
        leafletOutput("map")
      )
    ),
    fluidRow(
      box(
        width = 4,
        title = "Kategorizacija ulazaka",
        plotlyOutput("categories")
      ),
      box(width = 4,
          title = "Broj ulazaka po liniji",
          plotlyOutput("lines")),
      box(width = 4,
          title = "Broj ulazaka po danu",
          plotlyOutput("trend"))
    ))
  )


paletteBins <-
  c(0, 250, 500, 1000, 2500, 5000, 10000, 12500, 15000, 17500)

colorPalette <- colorBin(
  palette = "Blues",
  domain = 0:17500,
  na.color = "transparent",
  bins = paletteBins
)

stanice <-
  read_excel("GPP.xlsx", sheet = "STANICE", na = "NULL")
stanice$rad <- rescale(stanice$BROJ, to = c(5, 15))

dani <- read_excel("GPP.xlsx", sheet = "DANI", na = "NULL")
dani$DAN <- as.Date(dani$DAN, format = '%d%m%y')
svi <- read_excel("GPP.xlsx", sheet = "SVI", na = "NULL")
svi$DAN <- as.Date(svi$DAN, format = '%d%m%y')

stanice_kategorije_dan <-
  read_excel("GPP.xlsx", sheet = "STANICE_KATEGORIJE_DAN", na = "NULL")
stanice_kategorije_dan$DAN <-
  as.Date(stanice_kategorije_dan$DAN, format = '%d%m%y')

stanice_linije_dan <-
  read_excel("GPP.xlsx", sheet = "STANICE_LINIJE_DAN", na = "NULL")
stanice_linije_dan$DAN <-
  as.Date(stanice_linije_dan$DAN, format = '%d%m%y')

stanice_dan <-
  read_excel("GPP.xlsx", sheet = "STANICE_DAN", na = "NULL")
stanice_dan$DAN <- as.Date(stanice_dan$DAN, format = '%d%m%y')


#shiny server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(data = as.data.frame(stanice)) %>%
      addTiles()  %>%
      addCircleMarkers(
        clusterOptions = markerClusterOptions(),
        radius = ~ rad,
        weight = 1,
        opacity = 1,
        color = "black",
        layerId = ~ SIFRA,
        fillColor = ~ colorPalette(BROJ),
        fillOpacity = ~ rad,
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        label = ~ as.character(paste(NAZIV, "-", BROJ, "putnika")),
        popup = ~ as.character(paste(NAZIV, "-", BROJ, "putnika"))
      ) %>%
      fitBounds( ~ min(LONGITUDE),
                 ~ min(LATITUDE),
                 ~ max(LONGITUDE),
                 ~ max(LATITUDE)) %>%
      addLegend(
        position = "bottomright",
        pal = colorPalette,
        values = ~ BROJ,
        opacity = 1,
        title = "Broj putnika"      )
  })
  
  output$hourDist <- renderPlotly({
    validacije <- dani[dani$DAN == input$date, 2:3]
    fig <- plot_ly()
    fig <- layout(
      p = fig,
      showlegend = FALSE,
      xaxis = list(
        zeroline = FALSE,
        showline = FALSE,
        autotick = FALSE,
        range = c(0, 23),
        ticks = "outside",
        dtick = 1,
        title = "Sat"
      ),
      yaxis = list(title = "Broj ulazaka")
    )
    fig <- add_trace(
      p = fig,
      data = svi,
      name = "",
      x = ~ SAT,
      y = ~ BROJ,
      color = I("#00BFC4"),
      opacity = 0.1,
      mode = "lines",
      type = "scatter",
      hoverinfo = "skip"
    )
    fig <-
      add_trace(
        p = fig,
        x = ~ SAT,
        y = ~ BROJ,
        color = I("#F8766D"),
        opacity = 1,
        data = validacije,
        mode = "lines+markers",
        type = "scatter",
        hoverinfo = "closest"
      )
    toWebGL(fig)
  })
  
  output$categories <- renderPlotly({
    click <- input$map_marker_click
    if (is.null(click))
      return()
    
    stanica <- click$id
    dan <- input$date
    podatci <-
      stanice_kategorije_dan[stanice_kategorije_dan$DAN == dan &
                               stanice_kategorije_dan$STANICA == stanica, 3:4]
    if (prod(dim(podatci)) == 0)
      return()
    
    fig <-
      plot_ly(
        podatci,
        labels = ~ KATEGORIJA,
        values = ~ BROJ,
        type = 'pie'
      )
    toWebGL(fig)
  })
  
  output$trend <- renderPlotly({
    click <- input$map_marker_click
    if (is.null(click))
      return()
    
    stanica <- click$id
    dan <- input$date
    
    podatci <<- stanice_dan[stanice_dan$STANICA == stanica, 2:3]
    
    if (prod(dim(podatci)) == 0)
      return()
    
    fig <- plot_ly()
    fig <- layout(
      p = fig,
      showlegend = FALSE,
      xaxis = list(
        title = "Dan",
        range = c(as.Date("2019-01-14"), as.Date("2019-01-27")),
        ticks = "outside",
        type = 'date',
        dtick = 86400000,
        tickformat = "%d.%m."
      ),
      yaxis = list(range = c(0, max(podatci$BROJ)), title = "Broj ulazaka")
    )
    fig <- add_trace(
      p = fig,
      data = podatci,
      name = " ",
      x = ~ DAN,
      y = ~ BROJ,
      color = I("#00BFC4"),
      hovertemplate = '%{y}<extra></extra>',
      width = 1,
      mode = "lines+markers",
      type = "scatter"
    )
    fig <-
      add_trace(
        p = fig,
        x = ~ DAN,
        y = ~ BROJ,
        name = dan,
        color = I("#F8766D"),
        hovertemplate = '%{y}<extra></extra>',
        width = 3,
        data = podatci[podatci$DAN == dan,],
        mode = "markers",
        type = "scatter"
      )
    toWebGL(fig)
  })
  
  output$lines <- renderPlotly({
    click <- input$map_marker_click
    if (is.null(click))
      return()
    
    stanica <- click$id
    dan <- input$date
    podatci <-
      stanice_linije_dan[stanice_linije_dan$DAN == dan &
                           stanice_linije_dan$STANICA == stanica, 3:4]
    
    if (prod(dim(podatci)) == 0)
      return()
    
    fig <-
      plot_ly(
        data = podatci,
        x = ~ NAZIV,
        y = ~ BROJ,
        name = ~ NAZIV,
        text = ~ BROJ,
        textposition = 'auto',
        hovertemplate = '%{x}<br>%{y} putnika <extra></extra>',
        type = "bar"
      )
    fig <- layout(
      p = fig,
      showlegend = TRUE,
      xaxis = list(
        title = "",
        ticks = "",
        showticklabels = FALSE
      ),
      yaxis = list(range = c(0, max(podatci$BROJ)),
                   title = "Broj ulazaka"),
      legend = list(
        x = 0,
        y = -3,
        title = list(text = '<b>Linije</b>')
      )
    )
    
    toWebGL(fig)
  })
  
}

shinyApp(ui, server)