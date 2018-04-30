
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
library(RColorBrewer)

setwd("~/Documents/Github/you_spin_me_round/daniaApp")

fips <- read.csv(file="US_FIPS_Codes.csv", header=TRUE, sep=",")
fipsIllinois <- subset(fips, State == "Illinois")

illinois <- map_data("county") %>%
  filter(region == 'illinois')

tornadoes <- read.csv("tornadoes.csv")
tornadoes <- subset(tornadoes, st == "IL")
tornadoes$hr <- as.POSIXlt(tornadoes$time, format="%H:%M")$hour
tornadoes$countyNamef1 <- fipsIllinois$County.Name[match(tornadoes$f1, fipsIllinois$FIPS.County)]
tornadoes$countyNamef1 <- tolower(tornadoes$countyNamef1)
tornadoes$countyNamef2 <- fipsIllinois$County.Name[match(tornadoes$f2, fipsIllinois$FIPS.County)]
tornadoes$countyNamef2 <- tolower(tornadoes$countyNamef2)
tornadoes$countyNamef3 <- fipsIllinois$County.Name[match(tornadoes$f3, fipsIllinois$FIPS.County)]
tornadoes$countyNamef3 <- tolower(tornadoes$countyNamef3)
tornadoes$countyNamef4 <- fipsIllinois$County.Name[match(tornadoes$f4, fipsIllinois$FIPS.County)]
tornadoes$countyNamef4 <- tolower(tornadoes$countyNamef4)

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
                        (ifelse(tornadoesIL$loss <= 3, 
                                (ifelse(tornadoesIL$loss > 0, 1, 0)), tornadoesIL$loss - 2))))

#helper function to calculate the amount of tornadoes that had a certain loss range 
countLoss <- function(loss){
  ifelse(loss != 0, loss/loss, 1)
}

#get the yearly, monthly, and hourly loss tables 
yearlyloss <- tornadoes %>%
  group_by(yr, loss) %>%
  summarise(yrloss= sum(countLoss(loss)))

yearlyloss$loss[yearlyloss$loss == 0] <- "An Unknown"
yearlyloss$loss[yearlyloss$loss == 1] <- "Between 0 and 5,000"
yearlyloss$loss[yearlyloss$loss == 2] <- "Between 5,000 and 50,000"
yearlyloss$loss[yearlyloss$loss == 3] <- "Between 50,000 and 500,000"
yearlyloss$loss[yearlyloss$loss == 4] <- "Between 500,000 and 5,000,000"
yearlyloss$loss[yearlyloss$loss == 5] <- "Between 5,000,000 and 50,000,000"
yearlyloss$loss[yearlyloss$loss == 6] <- "Between 50,000,000 and 500,000,000"
yearlyloss$loss[yearlyloss$loss == 7] <- "Greater than 500,000,000"

monthlyloss <- tornadoes %>%
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

hourlyloss <- tornadoes %>%
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

tornadoesWithFips1 <- merge(illinois, tornadoes, by.x = "subregion", by.y = "countyNamef1")
tornadoesWithFips2 <- merge(illinois, tornadoes, by.x = "subregion", by.y = "countyNamef2")
tornadoesWithFips3 <- merge(illinois, tornadoes, by.x = "subregion", by.y = "countyNamef3")
tornadoesWithFips4 <- merge(illinois, tornadoes, by.x = "subregion", by.y = "countyNamef4")


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
  dynamic_bar_graph_stacked(yearlyloss, yearlyloss$yr, yearlyloss$yrloss, yearlyloss$loss, 
                            "Loss", "Year", "Tornadoes Per Year", "", "Year")
}

getAllTornadoDamagePerYear <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:13],by=list(tornadoes$yr), FUN=sum)
  names(tornadoData )[1]<-"year"
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
                            "Loss", "Month", "Tornadoes Per Month", "", "Month")
}


getAllTornadoDamagePerMonth <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:13],by=list(tornadoes$mo), FUN=sum)
  names(tornadoData )[1]<-"month"
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
                            "Loss", "Hour", "Tornadoes Per Hour", "", "Hour") 
}

getAllTornadoDamagePerDay <- function(tornadoes){
  tornadoData <- aggregate(tornadoes[,12:13],by=list(tornadoes$hr), FUN=sum)
  names(tornadoData )[1]<-"hour"
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
countyInjuriesMap <- ggplot(countyInj, aes(x = countyInj$long, y = countyInj$lat, group = group, fill = Injury)) + geom_polygon(color='black') + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_fill_gradient(low="#fee0d2",high="#99000d") + 
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggplotly(countyInjuriesMap)


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
countyDeathsMap <- ggplot(countyDeaths, aes(x = countyDeaths$long, y = countyDeaths$lat, group = group, fill = Fatalities)) + geom_polygon(color='black') + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_gradient(low="#fee0d2",high="#99000d") + 
  theme(plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
ggplotly(countyDeathsMap)

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
ggplotly(countyLossMap)


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
