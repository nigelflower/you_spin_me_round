library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Project 3: You Spin me Round", titleWidth = "100%"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "About", icon = icon("address-card")
            )
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "About",
                    h1(style = "font-size: 300%","Project 2: Learning to Fly"),
                    h4(style = "font-size: 100%","by: Daria Azhari, Nigel Flower, Jason Guo,  Ryan Nishimoto"),
                    h4(style = "font-size: 150%",a(href = "https://sites.google.com/uic.edu/nishimo1/cs424/project02", "Project Website")),
                    h2(style = "font-size: 200%","CS 424: Visualization and Visual Analytics"),
                    h4(style = "font-size: 150%",a(href = "https://www.evl.uic.edu/aej/424/", "Course website")),
                    h3(style = "font-size: 200%","Using this Application:"),
                    h4(style = "font-size: 150%","This project was a group effort between Dania Azhari, 
                  Nigel Flower, Jason Guo, and Ryan Nishimoto. This application 
                  included the following libraries:")
            ),
            
            tabItem(tabName=""
            
                    
            )
        )
    )
    
)

server <- function(input, output, session){
        
}

shinyApp(ui, server)