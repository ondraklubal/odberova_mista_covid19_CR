packs <- c("shiny", "shinydashboard", "dplyr", "leaflet", "tidyr")
for (x in packs){
  if (!x %in% installed.packages()){
    install.packages(x)
  }
}

library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(tidyr)

data <- readr::read_csv("your_working_directory/www/prehled-odberovych-mist.csv") # place here your repository path
data <- data %>% 
  filter(!is.na(latitude), !is.na(longitude))

data <- data %>% 
  mutate_at(c('nasofaryngealni_odber','orofaryngealni_odber', "antigenni_odber", "drive_in"), ~replace_na(.,0))


print(length(unique(data$odberove_misto_id)))

ui <- fluidPage(
  h1("Odběrová místa na COVID-19 - ČR"),
  absolutePanel(
    top = 80, right = 30, bottom = "auto", left = "auto", width = 300, height = "auto", draggable = F,
    wellPanel(
      strong("Vyberte odběrová místa:"),
      checkboxInput("drivein", "Pouze drive-in", value = FALSE),
      checkboxGroupInput("typ_odberu", "Typ odběru", selected = "nasofaryngealni_odber", 
                         choiceValues = names(data)[9:11], choiceNames = c("Nasofaryngeální", "Orofaryngeální", "Antigenní")),
      sliderInput("kapacita", "Odběrová kapacita", value = c(0, max(data$testovaci_kapacita)), min = min(data$testovaci_kapacita), max = max(data$testovaci_kapacita), step = 100)
    )
    , style = "opacity: 1; z-index: 10;" # z-index modification - for offset from map
  ),
      leafletOutput("map", height = "700px"),
  p("Pokud se mapa oddálí a neukazuje žádné body, znamená to, že pod daným filtrem neexistuje žádné místo v datech.", style = "color: grey"),
      hr(),
      print("Součástí publikovaného seznamu nejsou odběrová místa ve zdravotnických zařízeních, kde jsou prováděny testy před přijetím pacienta v souvislosti s poskytováním neodkladné péče. 
            Mobilní odběrové týmy mají kapacitu pevně nastavenu (na hodnotu 20), protože není možné přesně určit tuto kapacitu."),
  tags$em("Aktualizováno: listopad 2023"),
  tags$br(),
      tags$a(href="https://data.gov.cz/datová-sada?iri=https%3A%2F%2Fdata.gov.cz%2Fzdroj%2Fdatové-sady%2F00024341%2F712cbb09a4b206956a5a619cad7fc5b7",
             "Zdroj dat: Národní otevřená data ČR")

)


server <- function(input, output){
  
  filtered_data <- reactive({
    data <- data %>% 
      filter(drive_in == input$drivein, testovaci_kapacita >= input$kapacita[1], testovaci_kapacita <= input$kapacita[2])
    
    ifelse(input$typ_odberu == "nasofaryngealni_odber", data <- data %>% filter(nasofaryngealni_odber == 1), data)
    ifelse(input$typ_odberu == "orofaryngealni_odber", data <- data %>% filter(orofaryngealni_odber == 1), data)
    ifelse(input$typ_odberu == "antigenni_odber", data <- data %>% filter(antigenni_odber == 1), data)
    
    data
  })
  
  output$map <- renderLeaflet({
    filtered_data() %>% 
      leaflet() %>%
      addTiles() %>% 
      addCircleMarkers(radius = log(filtered_data()$testovaci_kapacita), 
                       lng = filtered_data()$longitude, 
                       lat = filtered_data()$latitude,
                       popup = paste0("<b>", filtered_data()$odberove_misto_nazev, "</b>", 
                                      "<br>Testovací kapacita: ", filtered_data()$testovaci_kapacita),
                       clusterOptions = markerClusterOptions(showCoverageOnHover = F))
  })
}

shinyApp(ui, server)
