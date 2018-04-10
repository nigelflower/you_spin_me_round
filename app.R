library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)


tornadoes <- read.csv("tornadoes.csv")


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
                
                # TODO Implement tab boxes to group all of the relevant
                # visualizations together

                mainPanel(
                    tabsetPanel(
                        
                        tabPanel(title="Tornado Magnitudes by Year",
                            fluidRow(
                                box(title="Tornado Magnitudes by Year",
                                    plotOutput("year_magnitude"), width=12)
                            )
                        ),

                        tabPanel(title="Month",
                            fluidRow(
                                box(title="Tornado Magnitudes by Month",
                                    plotOutput("month_magnitude"), width=12)
                            )
                        ),
                        
                        tabPanel(title="Hour",
                            fluidRow(
                                box(title="Tornado Magnitudes by Hour",
                                plotOutput("hour_magnitude"), width=12)
                            )
                        ),
                        
                        tabPanel(title="Distance",
                            fluidRow(
                                box(title="Tornado Magnitude by Distance",
                                plotOutput("distance_magnitude"), width=12),
                            
                                box(title = "Distance of Tornado in Miles",
                                sliderInput("slider", "Number of observations:", 0, 250, c(0, 100))
                            )
                            )
                                 
                        )
                    
                    )
                )
                
                # fluidRow(
                #     box(title="Tornado Magnitudes by Year",
                #         plotOutput("year_magnitude"), width=12)
                # ),
                # 
                # fluidRow(
                #     box(title="Tornado Magnitudes by Month",
                #         plotOutput("month_magnitude"), width=12)
                # ),
                # 
                # fluidRow(
                #     box(title="Tornado Magnitudes by Hour",
                #         plotOutput("hour_magnitude"), width=12)
                # ),
                # 
                # fluidRow(
                #     box(title="Tornado Magnitude by Distance",
                #         plotOutput("distance_magnitude"), width=12),
                # 
                #     box(title = "Distance of Tornado in Miles",
                #         sliderInput("slider", "Number of observations:", 0, 250, c(0, 100))
                #     )
                # )
                
                
            ),
            
            tabItem(tabName="Damages"
            
            ),
            
            tabItem(tabName="Illinois"
                    
            )
        )
    )
    
)

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
        filtered_tornadoes <- subset(tornadoes, len >= input$slider[1] & len <= input$slider[2])
        filt_year_mag <- data.frame(table(filtered_tornadoes$yr, filtered_tornadoes$mag))
        
        ggplot(data=filt_year_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') + 
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Year") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude"))
        
    })
}

shinyApp(ui, server)