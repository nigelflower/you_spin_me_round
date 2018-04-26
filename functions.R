
library(ggplot2)
library(lubridate)
library(plotly)

setwd("github/you_spin_me_round")

tornadoes <- read.csv("tornadoes.csv")


# --------------------------- PART C BULLET POINTS -----------------------------
# 1.) table and chart showing the total number of tornadoes (and # and % in each 
# magnitude, including 'unknown') for each year in the records and the overall 
# totals 

getTornadoCountsYear <- function(tornadoes){
    table(tornadoes$yr)
    table(tornadoes$yr, tornadoes$mag)
    t(apply(table(tornadoes$yr, tornadoes$mag), 1, function(i) i / sum(i)))
}

year_mag <- data.frame(table(tornadoes$yr, tornadoes$mag))

ggplot(data=year_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') + 
    theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
    xlab("Year") + ylab("Total Tornadoes") + 
    guides(fill=guide_legend(title="Magnitude"))

magnitudes <-c("-9", "0", "1", "2", "3", "4", "5")
year_mag_per <- data.frame(t(apply(table(tornadoes$yr, tornadoes$mag), 1, function(i) i / sum(i))))
colnames(year_mag_per) <- magnitudes
melted_ymp <- melt(as.matrix(year_mag_per))

ggplot(data=melted_ymp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
    xlab("Year") + ylab("Percentage of Magnitudes")


# 2.) table and chart showing the total numbers (and # and % in each magnitude) 
# per month summed over all years

getTornadoCountsMonth <- function(tornadoes){
    table(tornadoes$mo)
    table(tornadoes$mo, tornadoes$mag)
    t(apply(table(tornadoes$mo, tornadoes$mag), 1, function(i) i / sum(i)))
}

mo_mag <- data.frame(table(tornadoes$mo, tornadoes$mag))

mo_mag_per <- data.frame(t(apply(table(tornadoes$mo, tornadoes$mag), 1, function(i) i / sum(i))))
colnames(mo_mag_per) <- magnitudes
melted_mmp <- melt(as.matrix(mo_mag_per))

ggplot(data=melted_mmp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
    xlab("Month") + ylab("Percentage of Magnitudes")


ggplot(data=mo_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') +
    theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
    xlab("Month") + ylab("Total Tornadoes") + 
    guides(fill=guide_legend(title="Magnitude"))


# 3.) table and chart showing the total numbers (and # and % in each magnitude) 
# per hour of the day summed over all years

getTornadoCountsHour <- function(tornadoes){
    # get the hours
    hours <- hour(strptime(tornadoes$time, "%H:%M:%S"))
    table(hours)
    table(hours, tornadoes$mag)
    t(apply(table(hours, tornadoes$mag), 1, function(i) i / sum(i)))
}

hours <- format(strptime(tornadoes$time, "%H:%M:%S"), "%I %p")

hour_mag <- data.frame(table(hours, tornadoes$mag))

ggplot(data=hour_mag, aes(x=hours, y=Freq, fill=factor(Var2))) + geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
    xlab("Hour of Day") + ylab("Total Tornadoes") + 
    guides(fill=guide_legend(title="Magnitude")) 


+
    scale_fill_manual(limits=c(0,23),
                       breaks=0:23,
                       labels=c(paste(0:11,"am"),
                                "12 pm",
                                paste(1:11,"pm")))

hour_mag_per <- data.frame(t(apply(table(hours, tornadoes$mag), 1, function(i) i / sum(i))))
colnames(hour_mag_per) <- magnitudes
melted_hmp <- melt(as.matrix(hour_mag_per))

ggplot(data=melted_hmp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
    xlab("Hours") + ylab("Percentage of Magnitudes") +
    guides(fill=guide_legend(title="Magnitude"))



# 4.) table and chart showing the total numbers (and # and % in each magnitude) 
# for a given distance range from Chicago summed over all years

getTornadoCountsDistance <- function(tornadoes, lowerBound, upperBound){
    filtered_tornadoes <- subset(tornadoes, len > lowerBound & len < upperBound)
    table(filtered_tornadoes$yr)
    table(filtered_tornadoes$yr, filtered_tornadoes$mag)
    t(apply(table(filtered_tornadoes$yr, filtered_tornadoes$mag), 1, function(i) i / sum(i)))
}

filtered_tornadoes <- subset(tornadoes, len >= 5 & len < 100)
filt_year_mag <- data.frame(table(filtered_tornadoes$yr, filtered_tornadoes$mag))

ggplot(data=filt_year_mag, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') + 
    theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
    xlab("Year") + ylab("Total Tornadoes") + 
    guides(fill=guide_legend(title="Magnitude"))


filt_year_mag_per <- data.frame(t(apply(table(filtered_tornadoes$yr, filtered_tornadoes$mag), 1, function(i) i / sum(i))))
#colnames(filt_year_mag_per) <- magnitudes
melted_fymp <- melt(as.matrix(filt_year_mag_per))

ggplot(data=melted_fymp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3)

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

hours <- format(strptime(tornadoes$time, "%H:%M:%S"), "%I %p")

hour_mag_per <- data.frame(t(apply(table(hours, tornadoes$mag), 1, function(i) i / sum(i))))
colnames(hour_mag_per) <- magnitudes
melted_hmp <- melt(as.matrix(hour_mag_per))

ggplot(data=melted_hmp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
    xlab("Hours") + ylab("Percentage of Magnitudes") +
    guides(fill=guide_legend(title="Magnitude")) +
    scale_x_continuous(limits=c(0,24),
                       breaks=0:12*2,
                       labels=c(paste(0:5*2,"am"),
                                "12 pm",
                                paste(7:11*2-12,"pm"), 
                                "0 am")) 

ggplot(data=melted_hmp, aes(x=Var1, y=value, color=factor(Var2))) + geom_line(size=3) +
    xlab("Hours") + ylab("Percentage of Magnitudes") +
    guides(fill=guide_legend(title="Magnitude")) +
    scale_x_continuous(limits=c(0,23),
                       breaks=0:11*2,
                       labels=c(0:11*2)) 




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



