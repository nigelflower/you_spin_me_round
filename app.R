library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin="black",
    dashboardHeader(title = "You Spin me Round"),
    
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
        
}

shinyApp(ui, server)