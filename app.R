library(scales)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)

# Dev Tool for GitHub
library(devtools)
library(plotly)
library(dplyr)
library(stringr)
library(reshape2)
library(plyr)

setwd("/Users/jason/Documents/GitHub/you_spin_me_round")
tornadoes <- read.csv("tornadoes.csv")

IL_Code <- 17

# Get IL tornadoes from file              
illinois_tornadoes <- subset(tornadoes, stf == IL_Code)

#combine all the tornadoes from the f1-f4 code counts excluding the 0th county
illinois_counties <- as.data.frame(table(a = c(illinois_tornadoes[,"f1"], illinois_tornadoes[,"f2"], illinois_tornadoes[,"f3"], illinois_tornadoes[,"f4"])))
illinois_counties <- illinois_counties[-c(1), ]

names(illinois_counties) <- c("County", "Frequency")



ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Project 3: You Spin me Round", titleWidth = "100%"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "About"),
            menuItem("Tornadoes", tabName="Tornadoes"),
            menuItem("Damages", tabName="Damages"),
            menuItem("Illinois", tabName="Illinois")
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
                      title = "Tornado County", solidHeader = TRUE, status = "primary", width = 5,
                             id = "countyTable",
                             dataTableOutput("countyTable")
                      ),
                    fluidRow(
                      title = "Tornado County", solidHeader = TRUE, status = "primary", width = 5,
                             id = "countyChart",
                             plotOutput("countyChart")
                    )
                    
            ),
            
            tabItem(tabName="Damages"
            
            ),
            
            tabItem(tabName="Illinois"
                    
            )
        )
    )
    
)

server <- function(input, output, session){
  output$countyTable <- renderDataTable({
    datatable(illinois_counties, 
              options = list(searching = FALSE, pageLength = 6, lengthChange = FALSE))
  })
  

  output$countyChart <- renderPlot({
    ggplot(illinois_counties, aes(x=illinois_counties$County, y=illinois_counties$Frequency))  +
      geom_bar(position="dodge", stat="identity", fill = "orange") + labs(x="County Number", y = "# of Tornadoes") + theme(axis.text.x = element_text(angle = 70, vjust=0.5))
  })
  
}

shinyApp(ui, server)