library(shiny)
library(leaflet)
library(jsonlite)
library(httr)
library(mapboxer)
library(mapboxapi)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
source("tableau-in-shiny-v1.0.R")

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
  title = div(
    class = "weather-box",
    textOutput("currentDateTime"),
    uiOutput("weatherIconUI"),
    textOutput("weatherDescription"),
    textOutput("weatherTemp"),
    div(class = "location", "Melbourne")
  ),
  id = "mynav",
  header = setUpTableauInShiny(),
  position = "fixed-top",
  tags$head(
    tags$script(src = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"),
    tags$link(href = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css", rel = "stylesheet"),
    tags$link(href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.2.0/css/all.min.css", rel = "stylesheet"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300&display=swap", rel = "stylesheet"),
    tags$link(href = " https://cdn.bootcdn.net/ajax/libs/pretty-checkbox/3.0.3/pretty-checkbox.css", rel = "stylesheet"),
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
      .tabpanel {
        padding: 0;
      }
      .weather-box {
        position: absolute;
        top:0;
        right: 0;
        padding: 0;
        border: none;
        text-align: center;
        width: 300px;
        height: 100%;
        display: flex;
        flex-direction: row;
        align-items: center;
        font-size: 12px;
        color: white;
        font-family: 'Poppins', sans-serif;
      }
      #weatherDescription{
        padding: 0 5px;
      }
      #weatherTemp{
        padding: 0 5px;
      }
      #location{
        padding: 0 5px;
      }
    "))
  ),
  tabPanel(
    "Home",
    class = "tabpanel",
    includeHTML("home.html")
  ),
  tabPanel(
    "Map",
    class = "tabpanel",
    includeHTML("map.html")
  ),
  tabPanel(
    "Charts",
    class = "tabpanel",
    tableauPublicViz(
      id = "tableauViz",
      url = "https://public.tableau.com/views/SampleTableauembedforShinyintegrationlab/Hospitalstreemap",
      width = "100%",
      height = "700px"
    )
  ),
)


server <- function(input, output, session) {
  output$currentDateTime <- renderText({
    currentDateTime <- Sys.time()
    format(currentDateTime, "%Y-%m-%d %H:%M")
  })

  autoInvalidate <- reactiveTimer(60 * 1000)

  observe({
    autoInvalidate()
  })

  weather_data <- reactivePoll(600000, session,
    checkFunc = function() {
      Sys.time()
    },
    valueFunc = function() {
      getWeatherData()
    }
  )
  observeEvent(input$mynav, {
    runjs('dispatchEvent(new Event("resize"))')
  })

  output$weatherTemp <- renderText({
    paste0(weather_data()$temperature, "°C")
  })

  output$weatherDescription <- renderText({
    weather_data()$description
  })

  output$weatherIconUI <- renderUI({
    icon_code <- weather_data()$icon
    icon_url <- paste0("https://openweathermap.org/img/wn/", icon_code, "@2x.png")
    tags$img(src = icon_url, alt = "Weather icon", class = "weather-icon", style = "height: 50px;")
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
