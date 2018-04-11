library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)

#setwd("github/you_spin_me_round")

tornadoes <- read.csv("tornadoes.csv")


ui <- dashboardPage(skin="black",
    dashboardHeader(title = "You Spin me Round"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "About"),
            menuItem("Tornadoes", tabName="Tornadoes"),
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
            
            tabItem(tabName="Tornadoes",
                
                fluidRow(
                    box(title="Tornado Magnitudes by Year", 
                        plotOutput("year_magnitude"), width=12)
                ),
                
                fluidRow(
                    box(title="Tornado Magnitudes by Month",
                        plotOutput("month_magnitude"), width=12)
                ),
                
                fluidRow(
                    box(title="Tornado Magnitudes by Hour",
                        plotOutput("hour_magnitude"), width=12)
                ),
                
                fluidRow(
                    box(title="Tornado Magnitude by Distance",
                        plotOutput("distance_magnitude"), width=12)
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
                      box(width = 6,
                          selectInput(inputId = "SelectState0", label = "State", choices = state.abb, selected = "IL"),
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
    
    output$month_magnitude <- renderPlot({
        mo_mag <- data.frame(table(tornadoes$mo, tornadoes$mag))
        
        ggplot(data=mo_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') +
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Month") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude"))
        
    })
    
    output$hour_magnitude <- renderPlot({
        hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))
        hour_mag <- data.frame(table(hours, tornadoes$mag))
        ggplot(data=hour_mag, aes(x=hours, y=Freq, fill=Var2)) + geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Hour of Day") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude"))
        
    })
    
    output$distance_magnitude <- renderPlot({
        filtered_tornadoes <- subset(tornadoes, len > 5 & len < 6)
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
      
      dataset <- subset(tornadoes, st == input$SelectState0)
      dataset <- subset(dataset, yr == input$Slider0)
      map <- leaflet(options = leafletOptions(zoomControl= FALSE)) %>% #, dragging = FALSE, minZoom = 6, maxZoom = 6)) %>%
        addTiles() %>% 
        addProviderTiles(providers$Stamen.TonerLite) %>%
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
        addProviderTiles(providers$Stamen.TonerLite) %>%
        setView(map, 
                lng = state1()[,"x"], 
                lat = state1()[,"y"], 
                zoom = 6) %>%
        addMarkers(lng = dataset[,"slon"], lat = dataset[,"slat"], popup = "start") %>%
        addMarkers(lng = dataset[,"elon"], lat = dataset[,"elat"], popup = "end")
      map
    })
}

shinyApp(ui, server)