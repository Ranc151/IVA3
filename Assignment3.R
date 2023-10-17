library(shiny)
library(leaflet)
library(jsonlite)
library(httr)
library(mapboxer)
library(mapboxapi)
source('tableau-in-shiny-v1.0.R')

weather_API <- "b013907dbac70d244667999a00a73f26"
mapbox_token <- "pk.eyJ1Ijoia3Vkb3VyYW4iLCJhIjoiY2xuZ3h0ZWtkMGE1dTJycGY1aDIyaXhybSJ9.eySchTKDLLD44tuXIISWSQ"
style_url <- "mapbox://styles/kudouran/clnqxxyct00v001pv5l1hfys5"
Sys.setenv(MAPBOX_TOKEN = mapbox_token)

getWeatherData <- function() {
  openweather_url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=Melbourne,au&units=metric&appid=", weather_API)
  response <- GET(openweather_url)
  if (status_code(response) == 200) {
    weather_data <- content(response, "parsed", encoding = "UTF-8")
    list(
      temperature = weather_data$main$temp,
      description = weather_data$weather[[1]]$description,
      icon = weather_data$weather[[1]]$icon
    )
  } else {
    list()
  }
}

ui <- navbarPage(
  header=setUpTableauInShiny(),
  title = "Melbourne Travel Guide",
  tags$head(
    tags$script(src = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"),
    tags$link(href = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css", rel = "stylesheet"),
    tags$style(HTML("
      .navbar {
        background-color: #0572e1;
      }
      .navbar-default .navbar-brand {
        color: #000000;
        font-size: 24px;
      }
      .navbar-default .navbar-nav > li > a {
        color: #FFFFFF;
        font-size: 16px boil;
      }
      .weather-box {
        border: 1px solid #ccc;
        border-radius: 10px;
        text-align: center;
        width: 100%;
        margin-bottom: 10px;
        background-color: #d9d8d9;
      }
      #weatherIconUI {
        font-size: 40px;
      }
      #weatherTemp {
        font-size: 24px;
        margin-top: 10px;
        margin-bottom: 10px;
      }
      #weatherDescription {
        font-size: 16px;
      }

      .location {
        font-size: 14px;
      }
    ")
  )),
  tabPanel("Real Time Map",
    fluidRow(
      column(2, 
        div(class = "weather-box",
          uiOutput("weatherIconUI"),
          textOutput("weatherTemp"),
          textOutput("weatherDescription"),
          div(class = "location", "Melbourne")
        ),
        div(class="weather-box","Show something get from Mapbox")
      ),
      column(10,
        tags$div(id = "mapboxContainer", style = "width: 100%; height: 700px;"),
        tags$script(HTML("
          mapboxgl.accessToken = 'pk.eyJ1IjoicmFuY2hlbiIsImEiOiJjbG5xemdmMm4weG1uMmpwZG0zMmFseWkyIn0.59mv2UvALzSr3S1I-YEX_A';
          var map = new mapboxgl.Map({
            container: 'mapboxContainer',
            style: 'mapbox://styles/ranchen/clnpxlat800up01pvemzqe4z3',
            center: [144.9631, -37.8136], 
            zoom: 10
          });
        "))
      )
    )
  ),
  
  tabPanel("Historical Data",
    tableauPublicViz(
      id='tableauViz',       
      url='https://public.tableau.com/views/SampleTableauembedforShinyintegrationlab/Hospitalstreemap',
      width='100%',
      height='700px'
    )
  )
)


server <- function(input, output, session) {
  weather_data <- reactivePoll(600000, session,
  checkFunc = function() {
    Sys.time()
  },
  valueFunc = function() {
    getWeatherData()
  }
  )
  
  output$weatherTemp <- renderText({
    paste0(weather_data()$temperature, "°C")
  })
  
  output$weatherDescription <- renderText({
    weather_data()$description
  })
  
  output$weatherIconUI <- renderUI({
    icon_code <- weather_data()$icon
    icon_url <- paste0("https://openweathermap.org/img/wn/",icon_code,"@2x.png")
    tags$img(src = icon_url, alt = "Weather icon", class = "weather-icon")
  })
  
}

shinyApp(ui, server,options = list(launch.browser=TRUE))