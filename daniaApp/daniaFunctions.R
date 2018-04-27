
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(stringr)
library(reshape2)
library(scales)
library(plyr)
library(maps)
library(plotly)

setwd("~/Documents/Github/you_spin_me_round/daniaApp")

fips <- read.csv(file="US_FIPS_Codes.csv", header=TRUE, sep=",")
fipsIllinois <- subset(fips, State == "Illinois")

illinois <- map_data("county") %>%
  filter(region == 'illinois')

tornadoes <- read.csv("tornadoes.csv")
tornadoes <- subset(tornadoes, st == "IL")
tornadoes$hr <- as.POSIXlt(tornadoes$time, format="%H:%M")$hour
tornadoes$countyName <- fipsIllinois$County.Name[match(tornadoes$f1, fipsIllinois$FIPS.County)]
tornadoes$countyName <- tolower(tornadoes$countyName)

#illinois = illinois[!duplicated(illinois$subregion),]
tornadoes <- merge(illinois, tornadoes, by.x = "subregion", by.y = "countyName")

#Changing the data for loss column of tornadoes to be categorical 0-7
tornadoes$loss <- ifelse(tornadoes$yr >= 2016, 
                        (ifelse(tornadoes$loss >  0 & tornadoes$loss < 5000,1,
                        (ifelse(tornadoes$loss >= 5000 & tornadoes$loss < 50000,2,
                        (ifelse(tornadoes$loss >= 50000 & tornadoes$loss < 500000,3,
                        (ifelse(tornadoes$loss >= 500000 & tornadoes$loss < 5000000,4,
                        (ifelse(tornadoes$loss >= 5000000 & tornadoes$loss < 50000000,5,
                        (ifelse(tornadoes$loss >= 50000000 & tornadoes$loss < 50000000,6,
                        (ifelse(tornadoes$loss >= 50000000,7 , 0)))))))))))))), 
                  ifelse(tornadoes$yr >= 1996, 
                        (ifelse(tornadoes$loss >  0 & tornadoes$loss < 0.005, 1,
                        (ifelse(tornadoes$loss >= 0.005 & tornadoes$loss < 0.05,2,
                        (ifelse(tornadoes$loss >= 0.05 & tornadoes$loss < 0.5,3,
                        (ifelse(tornadoes$loss >= 0.5 & tornadoes$loss < 5,4,
                        (ifelse(tornadoes$loss >= 5 & tornadoes$loss < 50,5,
                        (ifelse(tornadoes$loss >= 50 & tornadoes$loss < 500,6,
                        (ifelse(tornadoes$loss >= 500,7 , 0)))))))))))))), 
                  (ifelse(tornadoes$loss <= 3, 1, tornadoes$loss - 2))))

#helper function to calculate the amount of tornadoes that had a certain loss range 
countLoss <- function(loss){
  ifelse(loss != 0, loss/loss, 1)
}

#get the yearly, monthly, and hourly loss tables 
yearlyloss <- tornadoes %>%
  group_by(yr, loss) %>%
  summarise(yrloss= sum(countLoss(loss)))

monthlyloss <- tornadoes %>%
  group_by(mo, loss) %>%
  summarise(moloss= sum(countLoss(loss)))

hourlyloss <- tornadoes %>%
  group_by(hr, loss) %>%
  summarise(hrloss=sum(countLoss(loss)))

# --------------------------- PART C BULLET POINTS -----------------------------
# 1.) table and chart showing the total number of tornadoes (and # and % in each 
# magnitude, including 'unknown') for each year in the records and the overall 
# totals 

getTornadoCountsYear <- function(tornadoes){
  table(tornadoes$yr)
}

# 2.) table and chart showing the total numbers (and # and % in each magnitude) 
# per month summed over all years

getTornadoCountsMonth <- function(tornadoes){
  
}


# 3.) table and chart showing the total numbers (and # and % in each magnitude) 
# per hour of the day summed over all years

getTornadoCountsHour <- function(tornadoes){
  
}

# 4.) table and chart showing the total numbers (and # and % in each magnitude) 
# for a given distance range from Chicago summed over all years

getTornadoCountsDistance <- function(tornadoes, lowerBound, upperBound){
  
}

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
  #tornadoData <- aggregate(tornadoes[,14],by=list(tornadoes$yr), FUN=sum) 
  #names(tornadoData )[1]<-"year"
  #names(tornadoData )[2]<-"total"
  #dynamic_bar_graph(tornadoData,tornadoData$year, tornadoData$total,"Year", "Total Loss", "")
  dynamic_bar_graph_stacked(yearlyloss, yearlyloss$yr, yearlyloss$yrloss, yearlyloss$loss, "Loss", "Year", "Tornadoes Per Year", "Loss", "Year")
}

getAllTornadoDamagePerYear <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:13],by=list(tornadoes$yr), FUN=sum)
  names(tornadoData )[1]<-"year"
  
  #line chart or grouped bar chart better?
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

# 6.) table and chart showing the injuries, fatalities, loss per month summed 
# over all years

getTornadoInjuriesPerMonth <- function(tornadoes){
  
  tornadoData <- aggregate(tornadoes[,12],by=list(tornadoes$mo), FUN=sum) 
  names(tornadoData )[1]<-"month"
  names(tornadoData )[2]<-"total"
  dynamic_bar_graph(tornadoData,tornadoData$month, tornadoData$total,"Month", "Total Injuries", "")
}

getTornadoFatalitiesPerMonth <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,13],by=list(tornadoes$mo), FUN=sum) 
  names(tornadoData )[1]<-"month"
  names(tornadoData )[2]<-"total"
  dynamic_bar_graph(tornadoData,tornadoData$month, tornadoData$total,"Month", "Total Fatalities", "")
}

getTornadoLossPerMonth <- function(tornadoes){
  dynamic_bar_graph_stacked(monthlyloss, monthlyloss$mo, monthlyloss$moloss, monthlyloss$loss,
                            "Loss", "Month", "Tornadoes Per Month", "Loss", "Month")
}


getAllTornadoDamagePerMonth <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:13],by=list(tornadoes$mo), FUN=sum)
  names(tornadoData )[1]<-"month"
  dynamic_line_chart(tornadoData, tornadoData$month, 
                     tornadoData$inj, "Injuries", 
                     tornadoData$fat, "Fatalities", 
                     "Month", "Total Loss", "", "Type of Damage")
  dynamic_bar_graph_grouped(tornadoData, tornadoData$month, 
                            tornadoData$inj, "Injuries", 
                            tornadoData$fat, "Fatalities", 
                            "Month", "Total Damages", "", "Type of Damage")
}

getAllTornadoDamagePerMonthTable <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:14],by=list(tornadoes$mo), FUN=sum)
  names(tornadoData)[1:4] <- c("Month","Injuries","Fatalities","Losses")
  tornadoData
}

# 7.) table and chart showing the injuries, fatalities, loss per hour of the day 
# summed over all years


getTornadoInjuriesPerDay <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12],by=list(tornadoes$hr), FUN=sum) 
  names(tornadoData )[1]<-"hour"
  names(tornadoData )[2]<-"total"
  dynamic_bar_graph(tornadoData,tornadoData$hour, tornadoData$total,"Hour of Day", "Total Injuries", "")
}

getTornadoFatalitiesPerDay <- function(tornadoes){
  
  tornadoData <- aggregate(tornadoes[,13],by=list(tornadoes$hr), FUN=sum) 
  names(tornadoData )[1]<-"hour"
  names(tornadoData )[2]<-"total"
  dynamic_bar_graph(tornadoData,tornadoData$hour, tornadoData$total,"Hour of Day", "Total Fatalities", "")
}

getTornadoLossPerDay <- function(tornadoes){
  dynamic_bar_graph_stacked(hourlyloss, hourlyloss$hr, hourlyloss$hrloss, hourlyloss$loss,
                            "Loss", "Hour", "Tornadoes Per Hour", "Loss", "Hour") 
}

getAllTornadoDamagePerDay <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:13],by=list(tornadoes$hr), FUN=sum)
  names(tornadoData )[1]<-"hour"
  dynamic_line_chart(tornadoData, tornadoData$hour, 
                     tornadoData$inj, "Injuries", 
                     tornadoData$fat, "Fatalities", 
                     "Hour of Day", "Total Damages", "", "Type of Damage")
  dynamic_bar_graph_grouped(tornadoData, tornadoData$hour, 
                            tornadoData$inj, "Injuries", 
                            tornadoData$fat, "Fatalities", 
                            "Hour of Day", "Total Damages", "", "Type of Damage")
}


getAllTornadoDamagePerDayTable <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:14],by=list(tornadoes$hr), FUN=sum)
  names(tornadoData)[1:4] <- c("Hour","Injuries","Fatalities","Losses")
  tornadoData
}

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

dynamic_bar_graph_stacked <- function(data, x_axis, y_axis,group, label, 
                                      x_axis_label, y_axis_label, title, legend_title = "Legend"){
  plot_ly(data, x = x_axis, y = y_axis, type = 'bar', name = label, color= group, colors = 'Reds') %>%
    layout(xaxis = list(title = x_axis_label, dtick=1, tickangle=45),
           yaxis = list(title = y_axis_label),
           title = title,
           margin = list(b = 100),
           barmode = 'stack')
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


# 8.) table and chart showing which counties were most hit by tornadoes summed 
# over all years


# 9.) leaflet map showing all of the tornado tracks across Illinois


# 10.) allow the user to switch between 12 hour am/pm time to 24 our time 
# display easily and to switch between imperial and metric units easily


# --------------------------- PART B BULLET POINTS -----------------------------
# 1.) ability to filter the tornadoes visible on the leaflet map by magnitude, 
# being able to show or hide each of the magnitudes individually

# 2.) ability to filter the tornadoes visible on the leaflet map by width, length, 
# injuries, fatalities, loss, year

# 3.) allow user to see all of the tornado tracks on the map with their color/width 
# based on user selected magnitude, length, width, loss, injuries, fatalities

# 4.) show data (and path on the leaflet map) for any of the 10 most 
# powerful / destructive Illinois tornadoes (you should pick the appropriate 
# metric and defend it)


# --------------------------- PART A BULLET POINTS -----------------------------
# 1.) allow user to see data (injuries, fatalities, loss, total number of 
# tornadoes of each magnitude) on a per county basis for all the Illinois 
# counties on the map

countyInj <- aggregate(inj ~ subregion + lat + long + group + order, tornadoes, sum)
countyInj<- countyInj[order(countyInj$order),] 


p <- x %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat, color = ~Injuries, colors = c('#ffffff','#000000'),  
          hoverinfo = 'text', text = ~subregion) %>%
  add_polygons(line = list(width = 0.4)) %>%
  add_polygons(
    fillcolor = 'transparent',
    line = list(color = 'black', width = 0.5),
    showlegend = FALSE
  ) %>%
  layout(
    title = "Illinois Injuries by County",
    titlefont = list(size = 10),
    xaxis = list(title = "", showgrid = FALSE,
                 zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE,
                 zeroline = FALSE, showticklabels = FALSE)
  )

countyInjuriesMap <- ggplot(countyInj, aes(x = countyInj$long, y = countyInj$lat, group = group, fill = inj)) + geom_polygon(color='black')


countyDeaths <- aggregate(fat ~ subregion + lat + long + group + order, tornadoes, sum)
countyDeaths <- countyDeaths[order(countyDeaths$order),] 
countyDeathsMap <- ggplot(countyDeaths, aes(x = countyDeaths$long, y = countyDeaths$lat, group = group, fill = fat)) + geom_polygon(color='black')


# 2.) allow a user to compare the Illinois tabular data to data from any other 
# state that the user chooses (from a list of all 50 states) in tabular form


# 3.) allow the user to change the map background by choosing from 5 different 
# useful map types (note this may mean you need to change your color scheme)


# 4.) allow a user to 'play back' or 'step through' time year by years showing 
# the tornadoes for that year in the tables and on the map


# ---------------------------- GRAD BULLET POINTS ------------------------------
# 1.) allow a user to compare the Illinois data to data from any other state 
# they choose in map form as well as tabular and chart form (i.e. when the user 
# chooses another state you should bring up another map showing the tornado 
# paths in that state as well filtered by the controls in 'B')


# 2.) use the data to create a heat map for Illinois showing where it is more 
# or less safe to be regarding tornadoes

gcounty <- map_data("county")
head(gcounty)
