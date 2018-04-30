library(leaflet)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(maps)
library(reshape2)
library(DT)
library(scales)
library(plyr)
library(maps)
library(plotly)
library(RColorBrewer)

tornadoes <- read.csv("tornadoes.csv")
magnitudes <-c("-9", "0", "1", "2", "3", "4", "5")
hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))

# Maybe add in Thunderforest.SpinalMap for fun....
provider_tiles <- c("Stamen Toner", "Open Topo Map", "Thunderforest Landscape", "Esri World Imagery", "Stamen Watercolor")

############################## Some of Jasons data ##########################################

counties_names <- read.csv("counties.csv")
IL_Code <- 17

# Get IL tornadoes from file              
illinois_tornadoes <- subset(tornadoes, stf == IL_Code)

#combine all the tornadoes from the f1-f4 code counts excluding the 0th county
illinois_counties <- as.data.frame(table(a = c(illinois_tornadoes[,"f1"], illinois_tornadoes[,"f2"], illinois_tornadoes[,"f3"], illinois_tornadoes[,"f4"])))
illinois_counties <- illinois_counties[-c(1), ]
names(illinois_counties) <- c("Code", "Frequency")

countyInfo <- data.frame(County=counties_names$County, Frequency= illinois_counties$Frequency)

############################################
#sorting by largest magnitude tornadoes
magnitude_sorted <- illinois_tornadoes[order(-illinois_tornadoes[,11]),]
magnitude_sorted10 <- head(magnitude_sorted,10)

injuries_sorted <- illinois_tornadoes[order(-illinois_tornadoes[,12]),]
injuries_sorted10 <- head(injuries_sorted,10)

fatalities_sorted <- illinois_tornadoes[order(-illinois_tornadoes[,13]),]
fatalities_sorted10 <-head(fatalities_sorted, 10)

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
yearlyloss$loss[yearlyloss$loss == 0] <- "0:Unknown"
yearlyloss$loss[yearlyloss$loss == 1] <- "1:Between 0 and 5,000"
yearlyloss$loss[yearlyloss$loss == 2] <- "2:Between 5,000 and 50,000"
yearlyloss$loss[yearlyloss$loss == 3] <- "3:Between 50,000 and 500,000"
yearlyloss$loss[yearlyloss$loss == 4] <- "4:Between 500,000 and 5,000,000"
yearlyloss$loss[yearlyloss$loss == 5] <- "5:Between 5,000,000 and 50,000,000"
yearlyloss$loss[yearlyloss$loss == 6] <- "6:Between 50,000,000 and 500,000,000"
yearlyloss$loss[yearlyloss$loss == 7] <- "7:Greater than 500,000,000"

monthlyloss <- tornadoesIL %>%
    group_by(mo, loss) %>%
    summarise(moloss= sum(countLoss(loss)))

monthlyloss$loss[monthlyloss$loss == 0] <- "0:Unknown"
monthlyloss$loss[monthlyloss$loss == 1] <- "1:Between 0 and 5,000"
monthlyloss$loss[monthlyloss$loss == 2] <- "2:Between 5,000 and 50,000"
monthlyloss$loss[monthlyloss$loss == 3] <- "3:Between 50,000 and 500,000"
monthlyloss$loss[monthlyloss$loss == 4] <- "4:Between 500,000 and 5,000,000"
monthlyloss$loss[monthlyloss$loss == 5] <- "5:Between 5,000,000 and 50,000,000"
monthlyloss$loss[monthlyloss$loss == 6] <- "6:Between 50,000,000 and 500,000,000"
monthlyloss$loss[monthlyloss$loss == 7] <- "7:Greater than 500,000,000"

hourlyloss <- tornadoesIL %>%
    group_by(hr, loss) %>%
    summarise(hrloss=sum(countLoss(loss)))

hourlyloss$loss[hourlyloss$loss == 0] <- "0:Unknown"
hourlyloss$loss[hourlyloss$loss == 1] <- "1:Between 0 and 5,000"
hourlyloss$loss[hourlyloss$loss == 2] <- "2:Between 5,000 and 50,000"
hourlyloss$loss[hourlyloss$loss == 3] <- "3:Between 50,000 and 500,000"
hourlyloss$loss[hourlyloss$loss == 4] <- "4:Between 500,000 and 5,000,000"
hourlyloss$loss[hourlyloss$loss == 5] <- "5:Between 5,000,000 and 50,000,000"
hourlyloss$loss[hourlyloss$loss == 6] <- "6:Between 50,000,000 and 500,000,000"
hourlyloss$loss[hourlyloss$loss == 7] <- "7:Greater than 500,000,000"

tornadoesWithFips1 <- merge(illinois, tornadoesIL, by.x = "subregion", by.y = "countyNamef1")
tornadoesWithFips2 <- merge(illinois, tornadoesIL, by.x = "subregion", by.y = "countyNamef2")
tornadoesWithFips3 <- merge(illinois, tornadoesIL, by.x = "subregion", by.y = "countyNamef3")
tornadoesWithFips4 <- merge(illinois, tornadoesIL, by.x = "subregion", by.y = "countyNamef4")

#part A bullet 1 code

# --------------------------- PART A BULLET POINTS -----------------------------
# 1.) allow user to see data (injuries, fatalities, loss, total number of 
# tornadoes of each magnitude) on a per county basis for all the Illinois 
# counties on the map

countyInj <- aggregate(inj ~ subregion + lat + long + group + order, tornadoesWithFips1, sum)

countyInj2 <- aggregate(inj ~ subregion + lat + long + group + order, tornadoesWithFips2, sum)
names(countyInj2)[names(countyInj2) == "inj"] = "inj2" 
countyInj2 <- countyInj2[ , -which(names(countyInj2) %in% c("subregion", "lat", "long", "group"))]

countyInj3 <- aggregate(inj ~ subregion + lat + long + group + order, tornadoesWithFips3, sum)
names(countyInj3)[names(countyInj3) == "inj"] = "inj3" 
countyInj3 <- countyInj3[ , -which(names(countyInj3) %in% c("subregion", "lat", "long", "group"))]

countyInj4 <- aggregate(inj ~ subregion + lat + long + group + order, tornadoesWithFips4, sum)
names(countyInj4)[names(countyInj4) == "inj"] = "inj4" 
countyInj4 <- countyInj4[ , -which(names(countyInj4) %in% c("subregion", "lat", "long", "group"))]

countyInj <- merge(x = countyInj, y = countyInj2, by = "order", all.x=TRUE)
countyInj <- merge(x = countyInj, y = countyInj3, by = "order", all.x=TRUE)
countyInj <- merge(x = countyInj, y = countyInj4, by = "order", all.x=TRUE)

countyInj$inj2[is.na(countyInj$inj2)] <- 0
countyInj$inj3[is.na(countyInj$inj3)] <- 0
countyInj$inj4[is.na(countyInj$inj4)] <- 0
countyInj$inj <- countyInj$inj + countyInj$inj2 + countyInj$inj3 + countyInj$inj4 

countyInj<- countyInj[order(countyInj$order),] 
names(countyInj)[names(countyInj) == "inj"] = "Injury"
countyInjuriesMap <- ggplot(countyInj, aes(x = countyInj$long, y = countyInj$lat, group = group, fill = Injury, text = paste('County: ', countyInj$subregion))) + geom_polygon(color='black') + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
    scale_fill_gradient(high = "#132B43", low = "#56B1F7") + 
    theme(plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
#ggplotly(countyInjuriesMap)


#county deaths
countyDeaths <- aggregate(fat ~ subregion + lat + long + group + order, tornadoesWithFips1, sum)

countyDeaths2 <- aggregate(fat ~ subregion + lat + long + group + order, tornadoesWithFips2, sum)
names(countyDeaths2)[names(countyDeaths2) == "fat"] = "fat2" 
countyDeaths2 <- countyDeaths2[ , -which(names(countyDeaths2) %in% c("subregion", "lat", "long", "group"))]

countyDeaths3 <- aggregate(fat ~ subregion + lat + long + group + order, tornadoesWithFips3, sum)
names(countyDeaths3)[names(countyDeaths3) == "fat"] = "fat3" 
countyDeaths3 <- countyDeaths3[ , -which(names(countyDeaths3) %in% c("subregion", "lat", "long", "group"))]

countyDeaths4 <- aggregate(fat ~ subregion + lat + long + group + order, tornadoesWithFips4, sum)
names(countyDeaths4)[names(countyDeaths4) == "fat"] = "fat4" 
countyDeaths4 <- countyDeaths4[ , -which(names(countyDeaths4) %in% c("subregion", "lat", "long", "group"))]

countyDeaths <- merge(x = countyDeaths, y = countyDeaths2, by = "order", all.x=TRUE)
countyDeaths <- merge(x = countyDeaths, y = countyDeaths3, by = "order", all.x=TRUE)
countyDeaths <- merge(x = countyDeaths, y = countyDeaths4, by = "order", all.x=TRUE)

countyDeaths$fat2[is.na(countyDeaths$fat2)] <- 0
countyDeaths$fat3[is.na(countyDeaths$fat3)] <- 0
countyDeaths$fat4[is.na(countyDeaths$fat4)] <- 0
countyDeaths$fat <- countyDeaths$fat + countyDeaths$fat2 + countyDeaths$fat3 + countyDeaths$fat4 

countyDeaths <- countyDeaths[order(countyDeaths$order),] 
names(countyDeaths)[names(countyDeaths) == "fat"] = "Fatalities"
countyDeathsMap <- ggplot(countyDeaths, aes(x = countyDeaths$long, y = countyDeaths$lat, group = group, fill = Fatalities, text = paste('County: ', countyDeaths$subregion))) + geom_polygon(color='black') + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_gradient(high = "#132B43", low = "#56B1F7") +  
    theme(plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
#ggplotly(countyDeathsMap)

#county loss
countyLoss <- tornadoesWithFips1

countyLoss2 <- tornadoesWithFips2
names(countyLoss2)[names(countyLoss2) == "loss"] = "loss2"
countyLoss2 <- countyLoss2[,c("order","loss2")]

countyLoss3 <- tornadoesWithFips3
names(countyLoss3)[names(countyLoss3) == "loss"] = "loss3"
countyLoss3 <- countyLoss3[,c("order","loss3")]

countyLoss4 <- tornadoesWithFips4
names(countyLoss4)[names(countyLoss4) == "loss"] = "loss4"
countyLoss4 <- countyLoss4[,c("order","loss4")]

countyLoss <- merge(x = countyLoss, y = countyLoss2, by = "order", all.x=TRUE)
countyLoss <- merge(x = countyLoss, y = countyLoss3, by = "order", all.x=TRUE)
countyLoss <- merge(x = countyLoss, y = countyLoss4, by = "order", all.x=TRUE)

countyLoss$loss2[is.na(countyLoss$loss2)] <- 0
countyLoss$loss3[is.na(countyLoss$loss3)] <- 0
countyLoss$loss4[is.na(countyLoss$loss4)] <- 0

countyLoss$loss <- pmax(countyLoss$loss, countyLoss$loss2, countyLoss$loss3, countyLoss$loss4)
countyLoss <- countyLoss[order(countyLoss$order),] 
countyLoss$loss[countyLoss$loss == 0] <- "0: Unknown"
countyLoss$loss[countyLoss$loss == 1] <- "1: Between 0 and 5,000"
countyLoss$loss[countyLoss$loss == 2] <- "2: Between 5,000 and 50,000"
countyLoss$loss[countyLoss$loss == 3] <- "3: Between 50,000 and 500,000"
countyLoss$loss[countyLoss$loss == 4] <- "4: Between 500,000 and 5,000,000"
countyLoss$loss[countyLoss$loss == 5] <- "5: Between 5,000,000 and 50,000,000"
countyLoss$loss[countyLoss$loss == 6] <- "6: Between 50,000,000 and 500,000,000"
countyLoss$loss[countyLoss$loss == 7] <- "7: Greater than 500,000,000"
names(countyLoss)[names(countyLoss) == "loss"] = "Losses"

countyLossMap <- ggplot(countyLoss, aes(x = countyLoss$long, y = countyLoss$lat, group = group, fill = Losses, text = paste('County: ', countyLoss$subregion))) + geom_polygon(color='black') + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
    scale_fill_brewer(palette="Blues")+ theme(
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
#ggplotly(countyLossMap)


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
            text = ~paste(x_axis,
                          '<br> Number of Tornadoes: ', y_axis,
                          '<br> Loss Category: ', group)) %>%
        layout(xaxis = list(title = x_axis_label, dtick=1, tickangle=45),
               yaxis = list(title = y_axis_label),
               title = title,
               margin = list(b = 100),
               barmode = 'stack')
}

#tables and plots
getTornadoInjFatPerYearTable <- function(tornadoesIL){
    tornadoData <- aggregate(tornadoesIL[,12:13],by=list(tornadoesIL$yr), FUN=sum)
    names(tornadoData)[1:3] <- c("Year","Injuries","Fatalities")
    tornadoData
}

getTornadoLossPerYearTable <- function(yearlyloss){
    tornadoData <- yearlyloss
    names(tornadoData)[1:3] <- c("Year", "Loss Category", "Number of Tornadoes")
    tornadoData
}

getTornadoInjFatPerMonthTable <- function(tornadoesIL){
    tornadoData <- aggregate(tornadoesIL[,12:13],by=list(tornadoesIL$mo), FUN=sum)
    names(tornadoData)[1:3] <- c("Month","Injuries","Fatalities")
    tornadoData
}

getTornadoLossPerMonthTable <- function(monthlyloss){
    tornadoData <- monthlyloss
    names(tornadoData)[1:3] <- c("Month", "Loss Category", "Number of Tornadoes")
    tornadoData
}

getTornadoInjFatPerHourTable <- function(tornadoesIL){
    tornadoData <- aggregate(tornadoesIL[,12:13],by=list(tornadoesIL$hr), FUN=sum)
    names(tornadoData)[1:3] <- c("Hour","Injuries","Fatalities")
    tornadoData
}

getTornadoLossPerHourTable <- function(hourlyloss){
    tornadoData <- hourlyloss
    names(tornadoData)[1:3] <- c("Hour", "Loss Category", "Number of Tornadoes")
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
                                    h4(style = "font-size: 100%","by: Daria Azhari, Nigel Flower, Jason Guo,  Ryan Nishimoto"),
                                    h4(style = "font-size: 150%",a(href = "https://sites.google.com/uic.edu/nishimo1/cs424/project03", "Project Website")),
                                    h2(style = "font-size: 200%","CS 424: Visualization and Visual Analytics"),
                                    h4(style = "font-size: 150%",a(href = "https://www.evl.uic.edu/aej/424/", "Course website"))
                            ),
                            
                            tabItem(tabName="Year",
                                    # Render the Datatable
                                    column(4,
                                           box(title="Datatable", 
                                               dataTableOutput("year_table"), width=12, height="1000px")
                                    ),
                                    
                                    # Render the magnitudes by year
                                    column(4,
                                           box(title="Tornado Magnitudes by Year",
                                               plotOutput("year_magnitude"), width=12, height="1000px")
                                    ),
                                    
                                    # Render the percentages of magnitudes by year
                                    column(4,
                                           box(title="Percentage of Magnitudes by Year",
                                               plotOutput("year_magnitude_percentage"), width=12, height="1000px")
                                           
                                    )
                            ),
                            
                            tabItem(tabName="Month",
                                    
                                    # Render data table
                                    column(4,
                                        box(title="Datatable",
                                            dataTableOutput("month_table"), width=12, height="1000px")
                                    ),
                                    
                                    # Render magnitudes by month
                                    column(4,
                                       box(title="Tornado Magnitudes by Month",
                                           plotOutput("month_magnitude"), width=12, height="1000px")
                                           
                                    ),
                                    
                                    # Render percentages of magnitudes by month
                                    column(4,
                                       box(title="Percentage of Magnitudes by Month",
                                           plotOutput("month_magnitude_percentage"), width=12, height="1000px")
                                    )
                            ),
                            
                            tabItem(tabName="Hour",
                                    
                                    fluidRow(
                                        radioButtons("hour_radio", h4("Time Selection"),
                                                     choices=list("24 Hours" = 1, "AM/PM" = 2),
                                                     selected=1)
                                    ),
                                    
                                    column(4,
                                        box(title="Datatable",
                                            dataTableOutput("hour_table"), width=12, height="800px")
                                    ),
                                         
                                    column(4,
                                        box(title="Tornado Magnitudes by Hour",
                                           plotOutput("hour_magnitude"), width=12, height="800px")
                                    ),
                                    
                                    column(4,
                                        box(title="Percentage of Magnitudes by Hour",
                                           plotOutput("hour_magnitude_percentage"), width=12, height="800px")
                                    )
                            ),
                            
                            tabItem(tabName="Distance",
                                    
                                    fluidRow(
                                        box(title = "Distance of Tornado in Miles",
                                            sliderInput("slider", "Number of observations:", 0, 234, c(0, 100))
                                        )
                                    ),
                                    
                                    column(4,
                                        box(title="Datatable", 
                                            dataTableOutput("distance_table"), width=12, height="800px")       
                                    ),
                                    
                                    column(4,
                                       box(title="Percentage of Magnitudes by Distance",
                                           plotOutput("distance_magnitude"), width=12, height="800px")
                                       
                                    ),
                                    
                                    column(4,
                                       box(title="Percentage of Magnitudes by Distance",
                                           plotOutput("distance_magnitude_percentage"), width=12, height="800px")
                                       
                                    )
                            ),
                            tabItem(tabName="YearDamages",
                                    fluidRow(
                                        box(title = "Tornado Injuries Per Year in Illinois", solidHeader = TRUE, status = "primary", width = 6,
                                            dataTableOutput("yearInjFatTable")),
                                        box(title = "Tornado Loss Per Year in Illinois", solidHeader = TRUE, status = "primary", width = 6,
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
                            tabItem(tabName="MonthDamages",
                                    fluidRow(
                                        box(title = "Tornado Injuries Per Month in Illinois", solidHeader = TRUE, status = "primary", width = 6,
                                            dataTableOutput("monthInjFatTable")),
                                        box(title = "Tornado Loss Per Month in Illinois", solidHeader = TRUE, status = "primary", width = 6,
                                            dataTableOutput("monthLossTable"))
                                    ),
                                    fluidRow(
                                        box(title="Tornado Injuries and Fatalities Per Month",
                                            plotlyOutput("monthInjFatPlot"), width=12)
                                    ),
                                    
                                    fluidRow(
                                        box(title="Tornado Monetary Loss Range Per Month",
                                            plotlyOutput("monthLossPlot"), width=12)
                                    )
                                    
                            ),
                            tabItem(tabName="HourDamages",
                                    fluidRow(
                                        radioButtons("hour_damages_radio", h4("Time Selection"),
                                                     choices=list("24 Hours" = 1, "AM/PM" = 2),
                                                     selected=2),
                                        box(title = "Tornado Injuries Per Hour in Illinois", solidHeader = TRUE, status = "primary", width = 6,
                                            dataTableOutput("hourInjFatTable")),
                                        box(title = "Tornado Loss Per Hour in Illinois", solidHeader = TRUE, status = "primary", width = 6,
                                            dataTableOutput("hourLossTable"))
                                    ),
                                    fluidRow(
                                        box(title="Tornado Injuries and Fatalities Per Hour",
                                            plotlyOutput("hourInjFatPlot"), width=12)
                                    ),
                                    
                                    fluidRow(
                                        box(title="Tornado Monetary Loss Range Per Hour",
                                            plotlyOutput("hourLossPlot"), width=12)
                                    )
                                    
                            ),
                            tabItem(tabName="Illinois",
                                    fluidRow(
                                        box(title = "Tornado County Table", solidHeader = TRUE, status = "primary", width = 12,
                                            dataTableOutput("countyTable"))
                                    ),
                                    
                                    fluidRow(
                                        box(title = "Tornado Counties Graph", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("countyChart"))
                                    ),
                                    
                                    fluidRow(
                                        box(title = "Illinois 10 Most Powerful/Destructive tornadoes", solidHeader = TRUE, status = "primary", width = 12,
                                            selectInput("top10", "Choose to view by criteria:", choices = c('Magnitude'='1','Fatality'='2', 'Injury' = '3'), selected = 'Magnitude'),
                                            uiOutput("reset2"),
                                            leafletOutput("Leaf10Most")
                                        )
                                    ),
                                    fluidRow(
                                        box(title="Illinois Injuries Per County",
                                            plotlyOutput("injuryCountyPlot"), width=4),
                                        box(title="Illinois Fatalities Per County",
                                            plotlyOutput("fatalityCountyPlot"), width=4),
                                        box(title="Illinois Loss Per County",
                                            plotlyOutput("lossCountyPlot"), width=4)
                                    )
                                    
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
    
    output$year_table <- renderDataTable({
        data.frame(table(tornadoes$yr, tornadoes$mag))
    })
    
    output$year_magnitude <- renderPlot({
        year_mag <- data.frame(table(tornadoes$yr, tornadoes$mag))
        
        ggplot(data=year_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') + 
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Year") + ylab("Total Earthquakes") + 
            guides(fill=guide_legend(title="Magnitude")) + scale_fill_brewer(palette="Set3")
    })
    
    output$year_magnitude_percentage <- renderPlot({
        year_mag_per <- data.frame(t(apply(table(tornadoes$yr, tornadoes$mag), 1, function(i) i / sum(i))))
        colnames(year_mag_per) <- magnitudes
        melted_ymp <- melt(as.matrix(year_mag_per))
        
        ggplot(data=melted_ymp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
            xlab("Year") + ylab("Percentage of Magnitudes") + scale_color_brewer(palette="Set3")
        
    })
    
    output$month_table <- renderDataTable(
        data.frame(table(tornadoes$mo, tornadoes$mag))
    )
    
    output$month_magnitude <- renderPlot({
        mo_mag <- data.frame(table(tornadoes$mo, tornadoes$mag))
        
        ggplot(data=mo_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') +
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Month") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude")) + scale_fill_brewer(palette="Set3")
        
    })
    
    output$month_magnitude_percentage <- renderPlot({
        mo_mag_per <- data.frame(t(apply(table(tornadoes$mo, tornadoes$mag), 1, function(i) i / sum(i))))
        colnames(mo_mag_per) <- magnitudes
        melted_mmp <- melt(as.matrix(mo_mag_per))
        
        ggplot(data=melted_mmp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
            xlab("Month") + ylab("Percentage of Magnitudes") + scale_color_brewer(palette="Set3")
        
    })
    
    output$hour_table <- renderDataTable(
        hour_mag <- data.frame(table(hours, tornadoes$mag))
    )
    
    output$hour_magnitude <- renderPlot({
        # hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))
        hour_mag <- data.frame(table(hours, tornadoes$mag))
        ggplot(data=hour_mag, aes(x=hours, y=Freq, fill=Var2)) + geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Hour of Day") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude")) + scale_fill_brewer(palette="Set3")
        
    })
    
    output$hour_magnitude_percentage <- renderPlot({
        # hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))
        hour_mag_per <- data.frame(t(apply(table(hours, tornadoes$mag), 1, function(i) i / sum(i))))
        colnames(hour_mag_per) <- magnitudes
        melted_hmp <- melt(as.matrix(hour_mag_per))
        
        ggplot(data=melted_hmp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
            xlab("Hours") + ylab("Percentage of Magnitudes") +
            guides(fill=guide_legend(title="Magnitude")) + scale_color_brewer(palette="Set3")
    })
    
    output$distance_table <- renderDataTable(
        subset(tornadoes, len >= input$slider[1] & len <= input$slider[2])
    )
    
    output$distance_magnitude <- renderPlot({
        filtered_tornadoes <- subset(tornadoes, len >= input$slider[1] & len <= input$slider[2])
        filt_year_mag <- data.frame(table(filtered_tornadoes$yr, filtered_tornadoes$mag))
        
        ggplot(data=filt_year_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') + 
            theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
            xlab("Year") + ylab("Total Tornadoes") + 
            guides(fill=guide_legend(title="Magnitude")) + scale_fill_brewer(palette="Set3")
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
            addCircleMarkers(lng = dataset[,"slon"], lat = dataset[,"slat"], popup = "start", radius = 5, color = 'red') %>%
            addCircleMarkers(lng = dataset[,"elon"], lat = dataset[,"elat"], popup = "end", radius = 5, color = 'red')
        dataset <- subset(dataset,  elat != 0.00 & elon != 0.00)
        
        for(i in 1:nrow(dataset)){
            map <- addPolylines(map, lat = as.numeric(dataset[i, c(16, 18)]), lng = as.numeric(dataset[i, c(17, 19)]), weight=1)
        }
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
            addCircleMarkers(lng = dataset[,"slon"], lat = dataset[,"slat"], popup = "start", radius = 5, color = 'red') %>%
            addCircleMarkers(lng = dataset[,"elon"], lat = dataset[,"elat"], popup = "end", radius = 5, color = 'red')
        
        map
    })
    
    output$Leaf10Most <- renderLeaflet({
        
        # Select dataset by critera
        if(input$top10 == "1"){   ## if it is the magnitude
            dataset <- magnitude_sorted10
        }
        else if(input$top10 == "2"){ ## if it is the fatalities
            dataset <- fatalities_sorted10
        }
        else{ ##  if it is injuries
            dataset <- injuries_sorted10
        }
        
        map <- leaflet(options = leafletOptions(zoomControl= FALSE)) %>% #, dragging = FALSE, minZoom = 6, maxZoom = 6)) %>%
            addTiles() %>% 
            
            # Select leaflet provider tiles from user input
            addProviderTiles(providers$Stamen.TonerLite) %>%
            
            setView(map, 
                    lng = state1()[,"x"], 
                    lat = state1()[,"y"], 
                    zoom = 6) %>%
            addCircleMarkers(lng = dataset[,"slon"], lat = dataset[,"slat"], popup = "start", radius = 5, color = 'red') %>%
            addCircleMarkers(lng = dataset[,"elon"], lat = dataset[,"elat"], popup = "end", radius = 5, color = 'red')
        dataset <- subset(dataset,  elat != 0.00 & elon != 0.00)
        
        for(i in 1:nrow(dataset)){
            map <- addPolylines(map, lat = as.numeric(dataset[i, c(16, 18)]), lng = as.numeric(dataset[i, c(17, 19)]), weight=1)
        }
        
        map
    })
    
    output$distance_magnitude_percentage <- renderPlot({
        filtered_tornadoes <- subset(tornadoes, len >= input$slider[1] & len <= input$slider[2])
        filt_year_mag_per <- data.frame(t(apply(table(filtered_tornadoes$yr, filtered_tornadoes$mag), 1, function(i) i / sum(i))))
        #colnames(filt_year_mag_per) <- magnitudes
        melted_fymp <- melt(as.matrix(filt_year_mag_per))
        
        ggplot(data=melted_fymp, aes(x=Var1, y=value, color=factor(Var2))) + 
            geom_line(size=3) + xlab("Year") + ylab("Percentage of Magnitudes") + scale_color_brewer(palette="Set3")
        
    })
    
    output$countyTable <- renderDataTable({
        datatable(countyInfo, 
                  options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE))
    })
    
    
    output$countyChart <- renderPlot({
        ggplot(data = countyInfo, aes(x=countyInfo$County, y=countyInfo$Frequency))  +
            geom_bar(position="dodge", stat="identity", fill = "orange") + labs(x="County ", y = "# of Tornadoes") + theme(axis.text.x = element_text(angle = 90, vjust=0.5))
        
    })
    
    
    #Dania's output
    
    #data tables for part c bullets 6-8 for years, months, and hours
    output$yearInjFatTable <- renderDataTable(
        getTornadoInjFatPerYearTable(tornadoesIL), 
        options = list(orderClasses = TRUE,
                       pageLength = 10,  dom = 'tp')
    )
    
    output$yearLossTable <- renderDataTable(
        getTornadoLossPerYearTable(yearlyloss), 
        options = list(orderClasses = TRUE,
                       pageLength = 10,  dom = 'tp')
    )
    
    output$monthInjFatTable <- renderDataTable(
        getTornadoInjFatPerMonthTable(tornadoesIL), 
        options = list(orderClasses = TRUE,
                       pageLength = 10,  dom = 'tp')
    )
    
    output$monthLossTable <- renderDataTable(
        getTornadoLossPerMonthTable(monthlyloss), 
        options = list(orderClasses = TRUE,
                       pageLength = 10,  dom = 'tp')
    )
    
    output$hourInjFatTable <- renderDataTable(
        getTornadoInjFatPerHourTable(tornadoesIL), 
        options = list(orderClasses = TRUE,
                       pageLength = 10,  dom = 'tp')
    )
    
    output$hourLossTable <- renderDataTable(
        getTornadoLossPerHourTable(hourlyloss), 
        options = list(orderClasses = TRUE,
                       pageLength = 10,  dom = 'tp')
    )
    
    #plots for part c bullets 6-8 for years, months, and hours
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
    
    output$monthInjFatPlot <- renderPlotly({
        tornadoData <- aggregate(tornadoesIL[,12:13],by=list(tornadoesIL$mo), FUN=sum)
        names(tornadoData )[1]<-"month"
        dynamic_bar_graph_grouped(tornadoData, tornadoData$month, 
                                  tornadoData$inj, "Injuries", 
                                  tornadoData$fat, "Fatalities", 
                                  "Month", "Total Damages", "", "Type of Damage")
    })
    
    output$monthLossPlot <- renderPlotly({
        dynamic_bar_graph_stacked(monthlyloss, monthlyloss$mo, monthlyloss$moloss, monthlyloss$loss,
                                  "Loss", "Month", "Tornadoes Per Month", "", "Month")
        
    })
    
    
    output$hourInjFatPlot <- renderPlotly({
        if(input$hour_damages_radio == 2){
            tornadoesIL$hr <- format(strptime(tornadoesIL$hr, "%H"),"%I %p" )
            tornadoData <- aggregate(tornadoesIL[,12:13],by=list(tornadoesIL$hr), FUN=sum)
            names(tornadoData )[1]<-"hour"
            plot_ly(tornadoData, x =~hour, y = ~inj, type = 'bar', name = "Injuries", marker = list(color = redColorLight),
                    hoverinfo='text', text = ~paste('Total Injuries: ', tornadoData$inj,
                                                    '<br> Total Fatalities', tornadoData$fat)) %>%
                add_trace(tornadoData, ~hour, y = ~fat,  name = "Fatalities", marker = list(color = redColorDark)) %>%
                layout(xaxis = list(title = "Hour of Day", dtick=1, tickangle=45, categoryorder = "array",
                                    categoryarray = c("01 AM", "02 AM", "03 AM", "04 AM", "05 AM", "06 AM","07 AM", "08 AM", "09 AM", "10 AM", "11 AM", "12 PM", 
                                                      "01 PM", "02 PM", "03 PM", "04 PM", "05 PM", "06 PM","07 PM", "08 PM", "09 PM", "10 PM", "11 PM", "12 AM")),
                       yaxis = list(title = "Total Damages"),
                       title = "",
                       margin = list(b = 100),
                       barmode = 'group')
        }
        else{
            tornadoData <- aggregate(tornadoesIL[,12:13],by=list(tornadoesIL$hr), FUN=sum)
            names(tornadoData )[1]<-"hour"
            dynamic_bar_graph_grouped(tornadoData, tornadoData$hour, 
                                      tornadoData$inj, "Injuries", 
                                      tornadoData$fat, "Fatalities", 
                                      "Hour of Day", "Total Damages", "", "Type of Damage")
        }
    })
    
    output$hourLossPlot <- renderPlotly({
        if(input$hour_damages_radio == 2){
            hourlyloss$hr <- format(strptime(hourlyloss$hr, "%H"),"%I %p" )
            plot_ly(hourlyloss, x = hourlyloss$hr, y = hourlyloss$hrloss, type = 'bar', name = "Loss", color= hourlyloss$loss, name = hourlyloss$loss,colors = 'Reds',
                    legendgroup = ~hourlyloss$loss,
                    hoverinfo = 'text',
                    text = ~paste(hourlyloss$hr,
                                  '<br> Number of Tornadoes: ', hourlyloss$hrloss,
                                  '<br> Loss Category: ', hourlyloss$loss)) %>%
                layout(xaxis = list(title = "Hour", dtick=1, tickangle=45, categoryorder = "array",
                                    categoryarray = c("01 AM", "02 AM", "03 AM", "04 AM", "05 AM", "06 AM","07 AM", "08 AM", "09 AM", "10 AM", "11 AM", "12 PM", 
                                                      "01 PM", "02 PM", "03 PM", "04 PM", "05 PM", "06 PM","07 PM", "08 PM", "09 PM", "10 PM", "11 PM", "12 AM")),
                       yaxis = list(title = "Tornadoes Per Hour"),
                       title = "",
                       margin = list(b = 100),
                       barmode = 'stack')
        }
        else{
            dynamic_bar_graph_stacked(hourlyloss, hourlyloss$hr, hourlyloss$hrloss, hourlyloss$loss,
                                      "Loss", "Hour", "Tornadoes Per Hour", "", "Hour")   
        }
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
    
    output$injuryCountyPlot <- renderPlotly({
        ggplotly(countyInjuriesMap)
    })
    
    output$fatalityCountyPlot <- renderPlotly({
        ggplotly(countyDeathsMap)
    })
    
    output$lossCountyPlot <- renderPlotly({
        ggplotly(countyLossMap)
    })
}

shinyApp(ui, server)