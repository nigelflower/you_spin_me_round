library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(leaflet)
library(datasets)
library(plotly)

#setwd("github/you_spin_me_round")

tornadoes <- read.csv("tornadoes.csv")
tornadoes$hr <- as.POSIXlt(tornadoes$time, format="%H:%M")$hour


### graph functions

blueColorLight <- "#67b8df"
blueColorDark <- "#3d6e85"
redColorLight <- "#ec5757"
redColorDark <- "#762b2b"

dynamic_bar_graph <- function(data,x_axis, y_axis,x_label, y_label, title){
  plot_ly(data, x = x_axis, y = y_axis, type = 'bar', color=I(redColorLight)) %>%
    layout(title = title,
           xaxis = list(title = x_label,dtick=1,tickangle=45),
           yaxis = list(title = y_label))
}

dynamic_bar_graph_grouped <- function(data, x_axis, y_axis1, label1, y_axis2, label2, 
                                      x_axis_label, y_axis_label, title, legend_title = "Legend"){
  plot_ly(data, x = x_axis, y = y_axis1, type = 'bar', name = label1, marker = list(color = redColorLight)) %>%
    add_trace(data=data, x = x_axis, y = y_axis2,  name = label2, marker = list(color = redColorDark)) %>%
    layout(xaxis = list(title = x_axis_label, dtick=1, tickangle=45),
           yaxis = list(title = y_axis_label),
           title = title,
           margin = list(b = 100),
           barmode = 'group')
}

dynamic_line_chart <- function(data, x_axis, y_axis1, label1, y_axis2, label2, 
                               x_axis_label, y_axis_label, title, legend_title = "Legend"){
  
  line_chart <- plot_ly(data, x = x_axis, y =  y_axis1, name = label1, type = 'scatter',mode = 'lines+markers')
  
  line_chart <- line_chart %>%
    add_trace(data, x = x_axis,y = y_axis2, name = label2, mode = 'lines+markers') 
  
  line_chart <- line_chart %>%
    layout(title = title,
           xaxis = list(title = x_axis_label, dtick=1, tickangle=45),
           yaxis = list (title = y_axis_label))
  
  line_chart
}

####

# 5.) table and chart showing the injuries, fatalities, loss for each year in 
# the records
getTornadoInjuriesPerYear <- function(tornadoes){
  
  tornadoData <- aggregate(tornadoes[,12],by=list(tornadoes$yr), FUN=sum) 
  names(tornadoData )[1]<-"year"
  names(tornadoData )[2]<-"total"
  dynamic_bar_graph(tornadoData,tornadoData$year, tornadoData$total,"Year", "Total Injuries", "")
}

getTornadoFatalitiesPerYear <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,13],by=list(tornadoes$yr), FUN=sum) 
  names(tornadoData )[1]<-"year"
  names(tornadoData )[2]<-"total"
  dynamic_bar_graph(tornadoData,tornadoData$year, tornadoData$total,"Year", "Total Fatalities", "")
}

getTornadoLossPerYear <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,14],by=list(tornadoes$yr), FUN=sum) 
  names(tornadoData )[1]<-"year"
  names(tornadoData )[2]<-"total"
  dynamic_bar_graph(tornadoData,tornadoData$year, tornadoData$total,"Year", "Total Loss", "")
}

getAllTornadoDamagePerYear <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:13],by=list(tornadoes$yr), FUN=sum)
  names(tornadoData )[1]<-"year"
  dynamic_line_chart(tornadoData, tornadoData$year, 
                     tornadoData$inj, "Injuries", 
                     tornadoData$fat, "Fatalities", 
                     "Year", "Total Loss", "", "Type of Damage")
  dynamic_bar_graph_grouped(tornadoData, tornadoData$year, 
                            tornadoData$inj, "Injuries", 
                            tornadoData$fat, "Fatalities", 
                            "Year", "Total Damages", "", "Type of Damage")
}

getAllTornadoDamagePerYearTable <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:14],by=list(tornadoes$yr), FUN=sum)
  names(tornadoData)[1:4] <- c("Year","Injuries","Fatalities","Losses")
  tornadoData
}

#####


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
                        
                        tabItem(tabName="Tornadoes"
                               
                        ),
                        
                        tabItem(tabName="Damages",
                                fluidRow(
                                  box(width = 4,
                                      selectInput("variable", "Variable:",
                                                  c("Cylinders" = "cyl",
                                                    "Transmission" = "am",
                                                    "Gears" = "gear"))
                                  )
                                ),
                                fluidRow(
                                  box(title="Tornado Magnitude by Distance",
                                      width = 6,
                                      dataTableOutput("dataTable")
                                  ),
                                  box(title="Tornado Magnitude by Distance",
                                      width = 6,
                                      plotlyOutput("data")
                                  )
                                )
                        ),
                        
                        tabItem(tabName="Illinois"
                                
                        ),
                        tabItem(tabName="TestLeaf"
                                
                        )
                      )
                    )
                    
)


server <- function(input, output){
  
  output$dataTable <- renderDataTable(
    getAllTornadoDamagePerYearTable(tornadoes),
    options = list(
      orderClasses = TRUE,
      pageLength = 10,
      dom = 'tp'
    )
  )
  
  output$data <- renderPlotly({
    getAllTornadoDamagePerYear(tornadoes)
  })
  
 
}

shinyApp(ui, server)