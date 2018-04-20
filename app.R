library(leaflet)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(maps)
library(reshape2)

tornadoes <- read.csv("tornadoes.csv")
magnitudes <-c("-9", "0", "1", "2", "3", "4", "5")
hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))

# Maybe add in Thunderforest.SpinalMap for fun....
provider_tiles <- c("Stamen Toner", "Open Topo Map", "Thunderforest Landscape", "Esri World Imagery", "Stamen Watercolor")

ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "You Spin me Round"),
                    
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("About", tabName = "About"),
                            menuItem("Tornadoes", tabName="Tornadoes",
                                     menuSubItem("Year", 
                                                 tabName="Year",
                                                 icon=icon("line-chart")),
                                     menuSubItem("Month",
                                                 tabName="Month",
                                                 icon=icon("calendar")),
                                     menuSubItem("Hour", 
                                                 tabName="Hour",
                                                 icon=icon("hourglass")),
                                     menuSubItem("Distance",
                                                 tabName="Distance",
                                                 icon=icon("plane"))
                            ),
                            menuItem("Damages", tabName="Damages"),
                            menuItem("Illinois", tabName="Illinois"),
                            menuItem("TestLeaf", tabName = "TestLeaf")
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
                            
                            tabItem(tabName="Year",
                                    fluidRow(
                                        box(title="Tornado Magnitudes by Year",
                                            plotOutput("year_magnitude"), width=12)
                                    ),
                                    
                                    fluidRow(
                                        box(title="Percentage of Magnitudes by Year",
                                            plotOutput("year_magnitude_percentage"), width=12)
                                    )
                            ),
                            
                            tabItem(tabName="Month",
                                    fluidRow(
                                        box(title="Tornado Magnitudes by Month",
                                            plotOutput("month_magnitude"), width=12)
                                    ),
                                    
                                    fluidRow(
                                        box(title="Percentage of Magnitudes by Month",
                                            plotOutput("month_magnitude_percentage"), width=12)
                                    )
                            ),
                            
                            tabItem(tabName="Hour",
                                    fluidRow(
                                        radioButtons("hour_radio", h4("Time Selection"),
                                                     choices=list("24 Hours" = 1, "AM/PM" = 2),
                                                     selected=1),
                                        
                                        box(title="Tornado Magnitudes by Hour",
                                            plotOutput("hour_magnitude"), width=12)
                                    ),
                                    
                                    fluidRow(
                                        box(title="Percentage of Magnitudes by Hour",
                                            plotOutput("hour_magnitude_percentage"), width=12)
                                    )
                            ),
                            
                            tabItem(tabName="Distance",
                                    fluidRow(
                                        box(title="Tornado Magnitude by Distance",
                                            plotOutput("distance_magnitude"), width=12)
                                    ),
                                    
                                    fluidRow(
                                        box(title="Percentage of Magnitudes by Distance",
                                            plotOutput("distance_magnitude_percentage"), width=12)
                                    ),
                                    
                                    fluidRow(
                                        box(title = "Distance of Tornado in Miles",
                                            sliderInput("slider", "Number of observations:", 0, 234, c(0, 100))
                                        )
                                    )
                            ),
                            
                            tabItem(tabName="Damages"
                                    
                            ),
                            
                            tabItem(tabName="Illinois"
                                    
                            ),
                            
                            tabItem(tabName="TestLeaf",
                                    h2("Testing area for Leaflet Plotting"),
                                    fluidRow(
                                        box(width = 12,
                                            sliderInput(inputId = "Slider0", label = "Year", min = 1950, max = 2016, value = 0, step = 1, animate = TRUE, sep = "")
                                        )
                                    ),
                                    
                                    fluidRow(
                                        # Filter by Magnitude
                                        column(2,
                                               checkboxGroupInput("magnitudeFilter",
                                                                  h3("Filter by Magnitude"),
                                                                  choices = list("-9" = -9, 
                                                                                 "0" = 0, 
                                                                                 "1" = 1, 
                                                                                 "2" = 2, 
                                                                                 "3" = 3, 
                                                                                 "4" = 4,
                                                                                 "5" = 5))
                                        ),
                                        
                                        # Filter by Width
                                        column(2,
                                               box(sliderInput("widthSlider", "Filter By Width", 0, 4576, 4576))
                                        ),
                                        
                                        # Filter by Length
                                        column(2,
                                               sliderInput("lengthSlider", "Filter By Length", 0, 234, 234)
                                        ),
                                        
                                        # Filter by Injuries
                                        column(2,
                                               sliderInput("injurySlider", "Filter By Injuries", 0, 1740, 1740)
                                        ),
                                        
                                        # Filter by Loss
                                        column(2,
                                               sliderInput("lossSlider", "Filter By Losses", 0, 22000000, 22000000)
                                        )
                                    ),
                                    
                                    fluidRow(
                                        box(width = 6,
                                            selectInput(inputId = "SelectState0", label = "State", choices = state.abb, selected = "IL"),
                                            selectInput(inputId = "MapSelect", label="Select Map Type", choices = provider_tiles, selected="Stamen Toner"),
                                            uiOutput("reset0"),
                                            leafletOutput("Leaf0")
                                        ),
                                        box(width = 6,
                                            selectInput(inputId = "SelectState1", label = "State", choices = state.abb, selected = "IL"),
                                            uiOutput("reset1"),
                                            leafletOutput("Leaf1")
                                        )
                                    )
                            )
                        )
                    )
)

# Ryan's variables pre-server

states <- data.frame(state.name,state.abb,state.center[1],state.center[2])
fips <- state.fips

server <- function(input, output, session){
    
    output$year_magnitude <- renderPlot({
        year_mag <- data.frame(table(tornadoes$yr, tornadoes$mag))
        
        ggplot(data=year_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') + 
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Year") + ylab("Total Earthquakes") + 
            guides(fill=guide_legend(title="Magnitude"))
    })
    
    output$year_magnitude_percentage <- renderPlot({
        year_mag_per <- data.frame(t(apply(table(tornadoes$yr, tornadoes$mag), 1, function(i) i / sum(i))))
        colnames(year_mag_per) <- magnitudes
        melted_ymp <- melt(as.matrix(year_mag_per))
        
        ggplot(data=melted_ymp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
            xlab("Year") + ylab("Percentage of Magnitudes")
        
    })
    
    output$month_magnitude <- renderPlot({
        mo_mag <- data.frame(table(tornadoes$mo, tornadoes$mag))
        
        ggplot(data=mo_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') +
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Month") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude"))
        
    })
    
    output$month_magnitude_percentage <- renderPlot({
        mo_mag_per <- data.frame(t(apply(table(tornadoes$mo, tornadoes$mag), 1, function(i) i / sum(i))))
        colnames(mo_mag_per) <- magnitudes
        melted_mmp <- melt(as.matrix(mo_mag_per))
        
        ggplot(data=melted_mmp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
            xlab("Month") + ylab("Percentage of Magnitudes")
        
    })
    
    output$hour_magnitude <- renderPlot({
        # hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))
        hour_mag <- data.frame(table(hours, tornadoes$mag))
        ggplot(data=hour_mag, aes(x=hours, y=Freq, fill=Var2)) + geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Hour of Day") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude"))
        
    })
    
    output$hour_magnitude_percentage <- renderPlot({
        # hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))
        hour_mag_per <- data.frame(t(apply(table(hours, tornadoes$mag), 1, function(i) i / sum(i))))
        colnames(hour_mag_per) <- magnitudes
        melted_hmp <- melt(as.matrix(hour_mag_per))
        
        ggplot(data=melted_hmp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
            xlab("Hours") + ylab("Percentage of Magnitudes") +
            guides(fill=guide_legend(title="Magnitude"))
    })
    
    output$distance_magnitude <- renderPlot({
        filtered_tornadoes <- subset(tornadoes, len >= input$slider[1] & len <= input$slider[2])
        filt_year_mag <- data.frame(table(filtered_tornadoes$yr, filtered_tornadoes$mag))
        
        ggplot(data=filt_year_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') + 
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Year") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude"))
    })
    
    
    # Ryan Leaflet Server Code
    
    # TODO: clean Reactive Variables
    reactiveData <- reactive({
        # Things to constrain by:
        #  Year
        #  width
        #  length
        #  injury
        #  fatalities
        #  Loss
        
        dataset <- subset()
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
        dataset <- subset(tornadoes, st == input$SelectState0)
        dataset <- subset(dataset, yr <= input$Slider0)
        
        # Subset by Magnitude
        mag_filter <- input$magnitudeFilter
        
        if(!is.null(mag_filter)){
            dataset <- subset(dataset, mag %in% mag_filter)
            print(strtoi(input$magnitudeFilter))
        }
        
        # Subset by Width
        wid_filter <- input$widthSlider
        dataset <- subset(dataset, wid < wid_filter)
        
        # Subset by Length
        len_filter <- input$lengthSlider
        dataset <- subset(dataset, len < len_filter)
        print(len_filter)
        
        # Subset by Injuries
        inj_filter <- input$injurySlider
        dataset <- subset(dataset, inj < inj_filter)
        
        # Subset by Loss
        loss_filter <- input$lossSlider
        dataset <- subset(dataset, loss < loss_filter)
        
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
                    zoom = 6) %>%
            addMarkers(lng = dataset[,"slon"], lat = dataset[,"slat"], popup = "start") %>%
            addMarkers(lng = dataset[,"elon"], lat = dataset[,"elat"], popup = "end")
        map
    })
    
    output$Leaf1 <- renderLeaflet({
        
        dataset <- subset(tornadoes, st == input$SelectState1)
        dataset <- subset(dataset, yr == input$Slider0)
        map <- leaflet(options = leafletOptions(zoomControl= FALSE)) %>% #, dragging = FALSE, minZoom = 6, maxZoom = 6)) %>%
            addTiles() %>% 
            
            # Select leaflet provider tiles from user input
            addProviderTiles(providers$Stamen.TonerLite) %>%
            
            setView(map, 
                    lng = state1()[,"x"], 
                    lat = state1()[,"y"], 
                    zoom = 6) %>%
            addMarkers(lng = dataset[,"slon"], lat = dataset[,"slat"], popup = "start") %>%
            addMarkers(lng = dataset[,"elon"], lat = dataset[,"elat"], popup = "end")
        map
    })
    
    output$distance_magnitude_percentage <- renderPlot({
        filtered_tornadoes <- subset(tornadoes, len >= input$slider[1] & len <= input$slider[2])
        filt_year_mag_per <- data.frame(t(apply(table(filtered_tornadoes$yr, filtered_tornadoes$mag), 1, function(i) i / sum(i))))
        #colnames(filt_year_mag_per) <- magnitudes
        melted_fymp <- melt(as.matrix(filt_year_mag_per))
        
        ggplot(data=melted_fymp, aes(x=Var1, y=value, color=factor(Var2))) + 
            geom_line(size=3) + xlab("Year") + ylab("Percentage of Magnitudes")
        
    })
}

shinyApp(ui, server)