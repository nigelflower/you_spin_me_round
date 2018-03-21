
library(ggplot2)

setwd("github/you_spin_me_round")

tornadoes <- read.csv("tornadoes.csv")


# --------------------------- PART C BULLET POINTS -----------------------------
# 1.) table and chart showing the total number of tornadoes (and # and % in each 
# magnitude, including 'unknown') for each year in the records and the overall 
# totals 

getTornadoCountsYear <- function(tornadoes){
    #table(tornadoes$yr)
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


# 6.) table and chart showing the injuries, fatalities, loss per month summed 
# over all years


# 7.) table and chart showing the injuries, fatalities, loss per hour of the day 
# summed over all years


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


