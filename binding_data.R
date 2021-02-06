#BINDING THE ALL THE DATA

names(data2013)[names(data2013) == "SCNTMONY"] <- "SCNTMNY1"
names(data2013)[names(data2013) == "SCNTMEAL"] <- "SCNTMEL1"

names(data2012)[names(data2012) == "EMPLOY"] <- "EMPLOY1"
names(data2012)[names(data2012) == "SCNTMONY"] <- "SCNTMNY1"
names(data2012)[names(data2012) == "SCNTMEAL"] <- "SCNTMEL1"
names(data2012)[names(data2012) == "X.RACE.G"] <- "X.RACE.G1"

total_data <- rbind(data2012,data2013,data2014,data2015)
