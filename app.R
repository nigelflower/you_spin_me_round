library(leaflet)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(maps)
library(reshape2)
library(heatmaply)
library(DT)
library(leaflet.extras)

tornadoes <- read.csv("tornadoes.csv")
magnitudes <-c("-9", "0", "1", "2", "3", "4", "5")
hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))


# Ryan's variables pre-server

# Read in lat/lon of each state's center
states <- data.frame(state.name,state.abb,state.center[1],state.center[2])
# Fix Alaska and Hawaii
states[state.abb == "AK",][3] <- -149.4937
states[state.abb == "AK",][4] <- 64.2008
states[state.abb == "HI",][3] <- -155.5828
states[state.abb == "HI",][4] <- 19.8968

# Maybe add in Thunderforest.SpinalMap for fun....
provider_tiles <- c("Stamen Toner", "Open Topo Map", "Thunderforest Landscape", "Esri World Imagery", "Stamen Watercolor")

counties_names <- read.csv("counties.csv")

IL_Code <- 17

# Get IL tornadoes from file              
illinois_tornadoes <- subset(tornadoes, stf == IL_Code)

#combine all the tornadoes from the f1-f4 code counts excluding the 0th county
illinois_counties <- as.data.frame(table(a = c(illinois_tornadoes[,"f1"], illinois_tornadoes[,"f2"], illinois_tornadoes[,"f3"], illinois_tornadoes[,"f4"])))
illinois_counties <- illinois_counties[-c(1), ]
names(illinois_counties) <- c("Code", "Frequency")

# I cant convert the county number to name
#
#dataframe of counties with code
# setDT(illinois_counties)
# setDT(counties_names)
#
#illinois_counties[ counties_names, on = c("Code"), Code := i.County]

ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "You Spin me Round"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Compare States", tabName = "CompareStates")
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "About",
                                h1(style = "font-size: 300%","Project 3: You Spin me Round"),
                                h4(style = "font-size: 100%","by: Daria Azhari, Nigel Flower, Jason Guo,  Ryan Nishimoto"),
                                h4(style = "font-size: 150%",a(href = "https://sites.google.com/uic.edu/nishimo1/cs424/project03", "Project Website")),
                                h2(style = "font-size: 200%","CS 424: Visualization and Visual Analytics"),
                                h4(style = "font-size: 150%",a(href = "https://www.evl.uic.edu/aej/424/", "Course website"))
                        ),
                        
                        tabItem(tabName="CompareStates",
                                column(3,
                                       box(width = 12),
                                       box(width = 12)
                                ),
                                column(4,
                                  box(width = 12),
                                  box(width = 12)
                                ),
                                column(3,
                                  box(width = 12, height = 1000,
                                      selectInput(inputId = "SelectState0", label = "State", choices = state.abb, selected = "IL"),
                                      uiOutput("reset0"),
                                      leafletOutput("Leaf0",height = 800)
                                  ),  
                                  box(width = 12, height = 1000,
                                      selectInput(inputId = "SelectState1", label = "State", choices = state.abb, selected = "IL"),
                                      uiOutput("reset1"),
                                      leafletOutput("Leaf1",height = 800)
                                  )
                                ),
                                
                                column(2,
                                       sliderInput(inputId = "compYear", label = "Year", min = 1950, max = 2016, value = 0, step = 1, animate = TRUE, sep = ""),
                                       sliderInput(inputId = 'compMonth', label = "Month(s)", min = 1, max = 12, value = c(1,12), step = 1, animate = TRUE, sep = ""),
                                       selectInput(inputId = "MapSelect", label="Select Map Type", choices = provider_tiles, selected="Stamen Toner"),
                                       checkboxGroupInput("magnitudeFilter",
                                                          h3("Filter by Magnitude"),
                                                          choices = list("unknown" = -9, 
                                                                         "0" = 0, 
                                                                         "1" = 1, 
                                                                         "2" = 2, 
                                                                         "3" = 3, 
                                                                         "4" = 4,
                                                                         "5" = 5),inline = TRUE),
                                       sliderInput("compWidth", "Filter By Width", min = 0, max = 4576, c(0, 4576)),
                                       sliderInput("compLength", "Filter By Length", min = 0, max = 234, value = c(0,234)),
                                       sliderInput("compInj", "Filter By Injuries", min = 0, max = 1740, value = c(0,1740)),
                                       sliderInput("compLoss", "Filter By Losses", min = 0, max = 22000000, value = c(0,22000000), pre = "$", sep = "," )
                                       
                                )
                        )
                        
                      )

                )
)

server <- function(input, output, session){
  
  
  # Ryan Leaflet Server Code
  
  # TODO: clean Reactive Variables
  reactiveData <- reactive({
    # Things to constrain by:
    # Subset by Year
    dataset <- subset(tornadoes, yr == input$compYear)
    # Subset by Month
    dataset <- subset(dataset, mo >= input$compMonth[1] & mo <= input$compMonth[2])
    # Subset by Width
    wid_min <- input$compWidth[1]
    wid_max <- input$compWidth[2]
    dataset <- subset(dataset, wid >= wid_min & wid <= wid_max)
    # Subset by Length
    len_min <- input$compLength[1]
    len_max <- input$compLength[2]
    dataset <- subset(dataset, len >= len_min & len <= len_max)
    # Subset by Injuries
    inj_min <- input$compInj[1]
    inj_max <- input$compInj[2]
    dataset <- subset(dataset, inj >= inj_min & inj <= inj_max)
    #  fatalities
    
    # Subset by Loss
    loss_min <- input$compLoss[1]
    loss_max <- input$compLoss[2]
    dataset <- subset(dataset, loss >= loss_min & loss <= loss_max)
    
    # Subset by Magnitude
    mag_filter <- input$magnitudeFilter
    
    if(!is.null(mag_filter)){
      dataset <- subset(dataset, mag %in% mag_filter)
      print(strtoi(input$magnitudeFilter))
    }
    
    dataset
  })
  # Variables for selecting state and lat/lon (separate from tornado dataset)
  state0 <- reactive({
    states[state.abb == input$SelectState0,]
  })
  state1 <- reactive({
    states[state.abb == input$SelectState1,]
  })
  
  
  # Plot output
  output$Leaf0 <- renderLeaflet({
    # Subset by Year And State
    dataset <- reactiveData()
    dataset <- subset(dataset, st == input$SelectState0)
    
    # Remove zero data
    dataset <- subset(dataset, slat != 0 & slon != 0 & elat != 0 & elon != 0 )
    
    
    # Select Provider Tiles
    if(input$MapSelect == "Stamen Toner"){
      tiles <- providers$Stamen.Toner
    }
    else if(input$MapSelect == "Open Topo Map"){
      tiles <- providers$OpenTopoMap
      
    }
    else if(input$MapSelect == "Thunderforest Landscape"){
      tiles <- providers$Thunderforest.Landscape
      
    }
    else if(input$MapSelect == "Esri World Imagery"){
      tiles <- providers$Esri.WorldImagery
      
    }
    else if(input$MapSelect == "Stamen Watercolor"){
      tiles <- providers$Stamen.Watercolor
    }
    else{
      tiles <- providers$Stamen.Toner
    }
    
    
    map <- leaflet(options = leafletOptions(zoomControl= FALSE)) %>% #, dragging = FALSE, minZoom = 6, maxZoom = 6)) %>%
      addTiles() %>% 
      addProviderTiles(tiles) %>%
      setView(map, 
              lng = state0()[,"x"],
              lat = state0()[,"y"], 
              zoom = 7) 
    #addMarkers(lng = dataset[,"slon"], lat = dataset[,"slat"], popup = "start") %>%
    #addMarkers(lng = dataset[,"elon"], lat = dataset[,"elat"], popup = "end")
    for(i in 1:nrow(dataset)){
      map <- addPolylines(map, lat = as.numeric(dataset[i,c('slat','elat')]),lng = as.numeric(dataset[i,c('slon','elon')]),
                          opacity = .2*((as.numeric(dataset[i,'mag'])+1))
                          )
                          
    }
    map
  })
  
  output$Leaf1 <- renderLeaflet({
    dataset <- reactiveData()
    dataset <- subset(dataset, st == input$SelectState1)
    # Remove zero data
    dataset <- subset(dataset, slat != 0 & slon != 0 & elat != 0 & elon != 0 )
    
    # Select Provider Tiles
    if(input$MapSelect == "Stamen Toner"){
      tiles <- providers$Stamen.Toner
    }
    else if(input$MapSelect == "Open Topo Map"){
      tiles <- providers$OpenTopoMap
      
    }
    else if(input$MapSelect == "Thunderforest Landscape"){
      tiles <- providers$Thunderforest.Landscape
      
    }
    else if(input$MapSelect == "Esri World Imagery"){
      tiles <- providers$Esri.WorldImagery
      
    }
    else if(input$MapSelect == "Stamen Watercolor"){
      tiles <- providers$Stamen.Watercolor
    }
    else{
      tiles <- providers$Stamen.Toner
    }
    
    map <- leaflet(options = leafletOptions(zoomControl= FALSE)) %>% #, dragging = FALSE, minZoom = 6, maxZoom = 6)) %>%
      addTiles() %>% 
      addProviderTiles(tiles) %>%
      setView(map, 
              lng = state1()[,"x"], 
              lat = state1()[,"y"], 
              zoom = 6) 
      for(i in 1:nrow(dataset)){
        map <- addPolylines(map, lat = as.numeric(dataset[i,c('slat','elat')]),lng = as.numeric(dataset[i,c('slon','elon')]), 
                            weight = 3*(as.numeric(dataset[i,'mag'])+1)
                            )
      }
      map
  })
  
}

shinyApp(ui, server)