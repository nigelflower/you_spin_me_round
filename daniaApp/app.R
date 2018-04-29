library(leaflet)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(maps)
library(reshape2)
library(DT)

tornadoes <- read.csv("tornadoes.csv")
magnitudes <-c("-9", "0", "1", "2", "3", "4", "5")
hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))

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

countyInfo <- data.frame(County=counties_names$County, Frequency= illinois_counties$Frequency)

#dania's code
fips <- read.csv(file="US_FIPS_Codes.csv", header=TRUE, sep=",")
fipsIllinois <- subset(fips, State == "Illinois")

illinois <- map_data("county") %>%
  filter(region == 'illinois')


tornadoesIL <- subset(tornadoes, st == "IL")
tornadoesIL$hr <- as.POSIXlt(tornadoesIL$time, format="%H:%M")$hour
tornadoesIL$countyNamef1 <- fipsIllinois$County.Name[match(tornadoesIL$f1, fipsIllinois$FIPS.County)]
tornadoesIL$countyNamef1 <- tolower(tornadoesIL$countyNamef1)
tornadoesIL$countyNamef2 <- fipsIllinois$County.Name[match(tornadoesIL$f2, fipsIllinois$FIPS.County)]
tornadoesIL$countyNamef2 <- tolower(tornadoesIL$countyNamef2)
tornadoesIL$countyNamef3 <- fipsIllinois$County.Name[match(tornadoesIL$f3, fipsIllinois$FIPS.County)]
tornadoesIL$countyNamef3 <- tolower(tornadoesIL$countyNamef3)
tornadoesIL$countyNamef4 <- fipsIllinois$County.Name[match(tornadoesIL$f4, fipsIllinois$FIPS.County)]
tornadoesIL$countyNamef4 <- tolower(tornadoesIL$countyNamef4)

#Changing the data for loss column of tornadoes to be categorical 0-7
tornadoesIL$loss <- ifelse(tornadoesIL$yr >= 2016, 
                    (ifelse(tornadoesIL$loss >  0 & tornadoesIL$loss < 5000,1,
                    (ifelse(tornadoesIL$loss >= 5000 & tornadoesIL$loss < 50000,2,
                    (ifelse(tornadoesIL$loss >= 50000 & tornadoesIL$loss < 500000,3,
                    (ifelse(tornadoesIL$loss >= 500000 & tornadoesIL$loss < 5000000,4,
                    (ifelse(tornadoesIL$loss >= 5000000 & tornadoesIL$loss < 50000000,5,
                    (ifelse(tornadoesIL$loss >= 50000000 & tornadoesIL$loss < 50000000,6,
                    (ifelse(tornadoesIL$loss >= 50000000,7 , 0)))))))))))))), 
                  ifelse(tornadoesIL$yr >= 1996, 
                    (ifelse(tornadoesIL$loss >  0 & tornadoesIL$loss < 0.005, 1,
                    (ifelse(tornadoesIL$loss >= 0.005 & tornadoesIL$loss < 0.05,2,
                    (ifelse(tornadoesIL$loss >= 0.05 & tornadoesIL$loss < 0.5,3,
                    (ifelse(tornadoesIL$loss >= 0.5 & tornadoesIL$loss < 5,4,
                    (ifelse(tornadoesIL$loss >= 5 & tornadoesIL$loss < 50,5,
                    (ifelse(tornadoesIL$loss >= 50 & tornadoesIL$loss < 500,6,
                    (ifelse(tornadoesIL$loss >= 500,7 , 0)))))))))))))), 
                  (ifelse(tornadoesIL$loss <= 3, 
                    (ifelse(tornadoesIL$loss > 0, 1, 0)), tornadoesIL$loss - 2))))

#helper function to calculate the amount of tornadoes that had a certain loss range 
countLoss <- function(loss){
  ifelse(loss != 0, loss/loss, 1)
}

#get the yearly, monthly, and hourly loss tables 
yearlyloss <- tornadoesIL %>%
  group_by(yr, loss) %>%
  summarise(yrloss= sum(countLoss(loss)))

#to present categorically in the legend
yearlyloss$loss[yearlyloss$loss == 0] <- "An Unknown"
yearlyloss$loss[yearlyloss$loss == 1] <- "Between 0 and 5,000"
yearlyloss$loss[yearlyloss$loss == 2] <- "Between 5,000 and 50,000"
yearlyloss$loss[yearlyloss$loss == 3] <- "Between 50,000 and 500,000"
yearlyloss$loss[yearlyloss$loss == 4] <- "Between 500,000 and 5,000,000"
yearlyloss$loss[yearlyloss$loss == 5] <- "Between 5,000,000 and 50,000,000"
yearlyloss$loss[yearlyloss$loss == 6] <- "Between 50,000,000 and 500,000,000"
yearlyloss$loss[yearlyloss$loss == 7] <- "Greater than 500,000,000"

monthlyloss <- tornadoesIL %>%
  group_by(mo, loss) %>%
  summarise(moloss= sum(countLoss(loss)))

monthlyloss$loss[monthlyloss$loss == 0] <- "An Unknown"
monthlyloss$loss[monthlyloss$loss == 1] <- "Between 0 and 5,000"
monthlyloss$loss[monthlyloss$loss == 2] <- "Between 5,000 and 50,000"
monthlyloss$loss[monthlyloss$loss == 3] <- "Between 50,000 and 500,000"
monthlyloss$loss[monthlyloss$loss == 4] <- "Between 500,000 and 5,000,000"
monthlyloss$loss[monthlyloss$loss == 5] <- "Between 5,000,000 and 50,000,000"
monthlyloss$loss[monthlyloss$loss == 6] <- "Between 50,000,000 and 500,000,000"
monthlyloss$loss[monthlyloss$loss == 7] <- "Greater than 500,000,000"

hourlyloss <- tornadoesIL %>%
  group_by(hr, loss) %>%
  summarise(hrloss=sum(countLoss(loss)))

hourlyloss$loss[hourlyloss$loss == 0] <- "An Unknown"
hourlyloss$loss[hourlyloss$loss == 1] <- "Between 0 and 5,000"
hourlyloss$loss[hourlyloss$loss == 2] <- "Between 5,000 and 50,000"
hourlyloss$loss[hourlyloss$loss == 3] <- "Between 50,000 and 500,000"
hourlyloss$loss[hourlyloss$loss == 4] <- "Between 500,000 and 5,000,000"
hourlyloss$loss[hourlyloss$loss == 5] <- "Between 5,000,000 and 50,000,000"
hourlyloss$loss[hourlyloss$loss == 6] <- "Between 50,000,000 and 500,000,000"
hourlyloss$loss[hourlyloss$loss == 7] <- "Greater than 500,000,000"

tornadoesWithFips1 <- merge(illinois, tornadoesIL, by.x = "subregion", by.y = "countyNamef1")
tornadoesWithFips2 <- merge(illinois, tornadoesIL, by.x = "subregion", by.y = "countyNamef2")
tornadoesWithFips3 <- merge(illinois, tornadoesIL, by.x = "subregion", by.y = "countyNamef3")
tornadoesWithFips4 <- merge(illinois, tornadoesIL, by.x = "subregion", by.y = "countyNamef4")

#graphing colors and functions

blueColorLight <- "#67b8df"
blueColorDark <- "#3d6e85"
redColorLight <- "#ec5757"
redColorDark <- "#762b2b"

dynamic_bar_graph <- function(data,x_axis, y_axis,x_label, y_label, title){
  plot_ly(data, x = x_axis, y = y_axis, type = 'bar', color=I(redColorLight), 
          hoverinfo='text', text = ~paste('Total: ', y_axis)) %>%
    layout(title = title,
           xaxis = list(title = x_label,dtick=1,tickangle=45),
           yaxis = list(title = y_label))
}

dynamic_bar_graph_grouped <- function(data, x_axis, y_axis1, label1, y_axis2, label2, 
                                      x_axis_label, y_axis_label, title, legend_title = "Legend"){
  plot_ly(data, x = x_axis, y = y_axis1, type = 'bar', name = label1, marker = list(color = redColorLight),
          hoverinfo='text', text = ~paste('Total Injuries: ', y_axis1,
                                          '<br> Total Fatalities', y_axis2)) %>%
    add_trace(data=data, x = x_axis, y = y_axis2,  name = label2, marker = list(color = redColorDark)) %>%
    layout(xaxis = list(title = x_axis_label, dtick=1, tickangle=45),
           yaxis = list(title = y_axis_label),
           title = title,
           margin = list(b = 100),
           barmode = 'group')
}

dynamic_bar_graph_stacked <- function(data, x_axis, y_axis,group, label, 
                                      x_axis_label, y_axis_label, title, legend_title = "Legend"){
  plot_ly(data, x = x_axis, y = y_axis, type = 'bar', name = label, color= group, name = group,colors = 'Reds',
          legendgroup = ~group,
          hoverinfo = 'text',
          text = ~paste('Year: ', x_axis,
                        '<br> Number of Tornadoes: ', y_axis,
                        '<br> Loss Category: ', group)) %>%
    layout(xaxis = list(title = x_axis_label, dtick=1, tickangle=45),
           yaxis = list(title = y_axis_label),
           title = title,
           margin = list(b = 100),
           barmode = 'stack')
}

#tables and plots
getTornadoInjuriesPerYearTable <- function(tornadoesIL){
  tornadoData <- aggregate(tornadoesIL[,12],by=list(tornadoesIL$yr), FUN=sum)
  names(tornadoData)[1:2] <- c("Year","Injuries")
  tornadoData
}

getTornadoFatalitiesPerYearTable <- function(tornadoesIL){
  tornadoData <- aggregate(tornadoesIL[,13],by=list(tornadoesIL$yr), FUN=sum)
  names(tornadoData)[1:2] <- c("Year","Fatalities")
  tornadoData
}

getTornadoLossPerYearTable <- function(yearlyloss){
  tornadoData <- yearlyloss
  names(tornadoData)[1:3] <- c("Year", "Loss Category", "Number of Tornadoes")
  tornadoData
}


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
                        menuItem("Damages", tabName="Damages",
                                 menuSubItem("Year", 
                                             tabName="YearDamages",
                                             icon=icon("line-chart")),
                                 menuSubItem("Month",
                                             tabName="MonthDamages",
                                             icon=icon("calendar")),
                                 menuSubItem("Hour", 
                                             tabName="HourDamages",
                                             icon=icon("hourglass"))
                        ),
                        menuItem("Illinois", tabName="Illinois"),
                        menuItem("TestLeaf", tabName = "TestLeaf"),
                        menuItem("Heatmap", tabName="Heatmap")
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "About",
                                h1(style = "font-size: 300%","Project 3: You Spin me Round"),
                                h4(style = "font-size: 100%","by: Dania Azhari, Nigel Flower, Jason Guo,  Ryan Nishimoto"),
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
                                ),
                                fluidRow(
                                  box(title = "Tornado County Table", solidHeader = TRUE, status = "primary", width = 12,
                                      dataTableOutput("countyTable"))
                                ),
                                fluidRow(
                                  box(title = "Tornado Counties Graph", solidHeader = TRUE, status = "primary", width = 12,
                                      plotOutput("countyChart"))
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
                        
                        tabItem(tabName="YearDamages",
                                fluidRow(
                                  box(title = "Tornado Injuries Per Year in Illinois", solidHeader = TRUE, status = "primary", width = 4,
                                      dataTableOutput("yearInjuriesTable")),
                                  box(title = "Tornado Fatalities Per Year in Illinois", solidHeader = TRUE, status = "primary", width = 4,
                                      dataTableOutput("yearFatalitiesTable")),
                                  box(title = "Tornado Loss Per Year in Illinois", solidHeader = TRUE, status = "primary", width = 4,
                                      dataTableOutput("yearLossTable"))
                                ),
                                fluidRow(
                                  box(title="Tornado Injuries and Fatalities Per Year",
                                      plotlyOutput("yearInjFatPlot"), width=12)
                                ),
                                
                                fluidRow(
                                  box(title="Tornado Monetary Loss Range Per Year",
                                      plotlyOutput("yearLossPlot"), width=12)
                                )
                                
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
                        ),
                        
                        tabItem(tabName="Heatmap",
                                h2("Heatmap Plots for Illinois Tornadoes"),
                                
                                fluidRow(
                                  box(title="Heatmap of Illinois Tornadoes Starting Point",
                                      selectInput(inputId="HeatmapState0", label="Select State", choices=state.abb, selected="IL"),
                                      leafletOutput("heatmap0"), width=6),
                                  
                                  box(title="Heatmap of Illinois Tornadoes Ending Point",
                                      selectInput(inputId="HeatmapState1", label="Select State", choices=state.abb, selected="IL"),
                                      leafletOutput("heatmap1"), width=6)
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
  
  heatmapState0 <- reactive({
    states[state.abb == input$HeatmapState0,]
  })
  
  heatmapState1 <- reactive({
    states[state.abb == input$HeatmapState1,]
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
  
  output$heatmap0 <- renderLeaflet({
    
    # Subset by Year And State
    dataset <- subset(tornadoes, st == input$HeatmapState0)
    
    map <- leaflet(dataset) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(map, 
              lng = heatmapState0()[,"x"], 
              lat = heatmapState0()[,"y"], 
              zoom = 6) %>%
      addHeatmap(lng = ~slon, lat = ~slat, intensity = ~mag, blur = 20,
                 max = 0.001, radius = 8)
    map
  })
  
  output$heatmap1 <- renderLeaflet({
    # Subset by Year And State
    dataset <- subset(tornadoes, st == input$HeatmapState1)
    
    map <- leaflet(dataset) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(map, 
              lng = heatmapState1()[,"x"], 
              lat = heatmapState1()[,"y"], 
              zoom = 6) %>%
      addHeatmap(lng = ~elon, lat = ~elat, intensity = ~mag, blur = 20,
                 max = 0.001, radius = 8)
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
  
  output$countyTable <- renderDataTable({
    datatable(illinois_counties, 
              options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE))
  })
  
  
  output$countyChart <- renderPlot({
    ggplot(data = countyInfo, aes(x=countyInfo$County, y=countyInfo$Frequency))  +
      geom_bar(position="dodge", stat="identity", fill = "orange") + labs(x="County", y = "# of Tornadoes") + theme(axis.text.x = element_text(angle = 90, vjust=0.5))
    
  })
  
  
  #Dania's output
  output$yearInjuriesTable <- renderDataTable(
    getTornadoInjuriesPerYearTable(tornadoesIL), 
    options = list(orderClasses = TRUE,
                   pageLength = 10,  dom = 'tp')
  )
  
  output$yearFatalitiesTable <- renderDataTable(
    getTornadoFatalitiesPerYearTable(tornadoesIL), 
    options = list(orderClasses = TRUE,
                   pageLength = 10,  dom = 'tp')
  )
  
  output$yearLossTable <- renderDataTable(
    getTornadoLossPerYearTable(yearlyloss), 
    options = list(orderClasses = TRUE,
                   pageLength = 10,  dom = 'tp')
  )
  
  output$yearInjFatPlot <- renderPlotly({
    tornadoData <- aggregate(tornadoesIL[,12:13],by=list(tornadoesIL$yr), FUN=sum)
    names(tornadoData )[1]<-"year"
    dynamic_bar_graph_grouped(tornadoData, tornadoData$year, 
                              tornadoData$inj, "Injuries", 
                              tornadoData$fat, "Fatalities", 
                              "Year", "Total Damages", "", "Type of Damage")
  })
  
  output$yearLossPlot <- renderPlotly({
    dynamic_bar_graph_stacked(yearlyloss, yearlyloss$yr, yearlyloss$yrloss, yearlyloss$loss, 
                              "Loss", "Year", "Tornadoes Per Year", "", "Year")
    
  })
}

shinyApp(ui, server)