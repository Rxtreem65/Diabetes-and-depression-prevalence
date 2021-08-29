################################################################################

# Bindding all the years data

################################################################################

#BINDING THE ALL THE DATA

#changing the name of columns of year 2013
names(data2013)[names(data2013) == "SCNTMONY"] <- "SCNTMNY1"
names(data2013)[names(data2013) == "SCNTMEAL"] <- "SCNTMEL1"

#changing the name of columns of year 2012
names(data2012)[names(data2012) == "EMPLOY"] <- "EMPLOY1"
names(data2012)[names(data2012) == "SCNTMONY"] <- "SCNTMNY1"
names(data2012)[names(data2012) == "SCNTMEAL"] <- "SCNTMEL1"
names(data2012)[names(data2012) == "X.RACE.G"] <- "X.RACE.G1"

#binding data of all years in total_data
total_data <- rbind(data2012,data2013,data2014,data2015)


data201=total_data

#Processing Age group
table(data201$X.AGEG5YR)
table(data201$DIABAGE2)
data201 = data201[!is.na(data201$X.AGEG5YR),]
data201 = subset(data201, data201$X.AGEG5YR<14)
data201 = subset(data201, data201$DIABAGE2>29)
data201 = subset(data201, data201$DIABAGE2<=80)
table(data201$X.AGEG5YR)
nrow(data201)
table(data201$X.BMI5)
data201 = data201[!is.na(data201$X.BMI5),]
hist(data201$DIABAGE2, xlab = "diabetes age")
hist(data201$X.BMI5, xlab = "BMI")

total_data = data201
