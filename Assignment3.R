library(shiny)
library(leaflet)
library(jsonlite)
library(httr)
library(mapboxer)
library(mapboxapi)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(ggiraph)
library(tidyr)
library(dplyr)
library(stringr)
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
    div(id = "location", "Melbourne")
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
      .tabpanel-tableau{
        padding: 0;
        padding-top: 30px;
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
      #home {
        margin: 0;
        padding-top: 50px;
        width: auto;
        height: 100vh;
        background-image: linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url(./background.png);
        background-size: cover;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }

      #header {
        animation-name: animate__lightSpeedInLeft;
        animation-duration: 2s;
        text-align: center;
        font: 3.5em sans-serif;
        color: aliceblue;
      }

      #home p {
        padding: 2vh;
        font: 1em;
        color: azure;
        text-align: center;
      }

      .tab-button {
        width: 50%;
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        grid-gap: 20px;
      }

    .tab-button i {
        padding: 3px;
    }
      .tab-button button {
        height: 5vh;
        text-align: center;
        background: linear-gradient(to right, #cde6f1, #f0d8e6, #f5efbf, #95d9f9);
        background-size: 400%;
        border-radius: 10px;
        border: none;
        outline: none;
        z-index: 1;
        padding: 0;
      }

      .tab-button button::before {
        text-align: center;
        background: linear-gradient(to right, #03a9f4, #f441a5, #ffeb3b, #09a8f4);
        background-size: 400%;
        border-radius: 10px;
        z-index: -1;
        filter: blur(20px);
      }

      .tab-button button:hover {
        animation: streamer 4s infinite;
      }

      .tab-button button:hover::before {
        animation: streamer 8s infinite;
      }
      @keyframes streamer {
        100% {
            background-position: -400% 0;
        }
      }

      @keyframes animate__lightSpeedInLeft {
        0% {
            transform: translate3d(-100%, 0, 0) skewX(-30deg);
            opacity: 0;
        }

        60% {
            transform: skewX(20deg);
            opacity: 1;
        }

        80% {
            transform: skewX(-5deg);
            opacity: 1;
        }

        100% {
            transform: none;
            opacity: 1;
        }
      }
    "))
  ),
  tabPanel(
    "Home",
    class = "tabpanel",
    div(
      id = "home",
      div(
        h1(
          "MELBOURNE TOUR MAP",
          id = "header"
        ),
        p("Welcome to Melbourne Tour Map."),
      ),
      div(
        class = "tab-button",
        actionButton("mapIcon",
          id = "mapIcon",
          label = "Explore Melbourne Map", icon("map", lib = "font-awesome"),
          style = "color: #07ab0a;", onclick = "$('li:eq(2) a').tab('show');"
        ),
        actionButton("chartIcon",
          id = "chartIcon",
          label = "Analyse Data", icon("chart-simple", lib = "font-awesome"),
          style = "color: #c963ee;", onclick = "$('li:eq(3) a').tab('show');"
        )
      )
    ),
  ),
  tabPanel(
    "Map",
    class = "tabpanel",
    includeHTML("map.html")
  ),
  tabPanel(
    "Charts",
    class = "tabpanel-tableau",
    # verticalLayout
    verticalLayout(
      tableauPublicViz(
        id = "tableauViz1",
        url = 'https://public.tableau.com/views/melbournetraffic_16978952152700/Dashboard1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
        width = "90vw",
        height = "100%"
      ),
      splitLayout(
        tableauPublicViz(
          id = "tableauViz",
          url = 'https://public.tableau.com/views/melbournetraffic_16978952152700/Dashboard3?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link',
          width = "100%",
          height = "100%"
        ),
        girafeOutput('tableau_choce_data')
      ),
    ),
  ),
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

  autoInvalidate <- reactiveTimer(1000)

  currentTime <- reactive({
    autoInvalidate()
    Sys.time()
  })

  output$currentDateTime <- renderText({
    format(currentTime(), "%Y-%m-%d %H:%M:%S")
  })

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

  output$tableau_choce_data <- renderGirafe({
    # Find the name of the hospital clicked by the user
    df <- input$tableauViz_mark_selection_changed
    print(df)
    if(is.null(df)) return()
    if (!is.data.frame(df)) return()
    if (ncol(df) < 5) return()
    df <- df[, c(2, 3)]
    df <- df %>% rename(Trucks = names(df)[2])
    df <- df %>%
      mutate(Label = str_replace(Label, ".* (?=.* )", ""))
    df <- df %>% filter(!grepl("%null%", Trucks))
    df$Trucks <- as.numeric(df$Trucks)
    df <- df %>%
      group_by(Label) %>%  
      summarise(Trucks = sum(Trucks))
    p <- ggplot(df, aes(x = Label , y = Trucks)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      coord_flip() + # 
      labs(x = "Area", y = "Traffic Volume", title = "Traffic Volume by Area") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
    girafe(ggobj = p)
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
