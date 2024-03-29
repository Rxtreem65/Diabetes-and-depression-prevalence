################################################################################

# pre-processing variables of year 2012

################################################################################


library(SASxport)

data2012 <- read.xport("~/Downloads/GDM_data/LLCP2012.XPT")
fea <- c("DIABAGE2","X.AGEG5YR","SEX","X.RACE.G","X.BMI5","EXERANY2",
         "MENTHLTH","INCOME2","EMPLOY","EDUCA","SCNTMONY","SCNTMEAL",
         "RENTHOM1","DIABETE3","ADDEPEV2")
data2012 <- data2012[fea]
nrow(data2012)

#processing for diabetes diagnosis age
table(data2012$DIABAGE2)
data2012 = data2012[!is.na(data2012$DIABAGE2),] #removing missing values
hist(data2012$DIABAGE2, 
     main = "Histogram of age at which diabetes diagnosed",
     xlab = "Age at which respondent diagnoised with diabetes",
     ylab = "number of respondents") #histogram plot for diagnosed age
boxplot(data2012$DIABAGE2, main = "Boxplot of age at which diabetes diagnose")
mean_dia = mean(data2012$DIABAGE2) #mean of diabetes age
mean_dia
std_dia = sd(data2012$DIABAGE2) #standard deviation of diabetes age
std_dia
mdl = mean_dia-1.96*std_dia #lower interval of CI
mdh = mean_dia+1.96*std_dia #upper interval of CI
mdl
mdh
#CI 95% subset
data2012 = subset(data2012, data2012$DIABAGE2 >= 30) 
data2012 = subset(data2012, data2012$DIABAGE2 <= mdh)
table(data2012$DIABAGE2)
nrow(data2012)


#Processing Age group
table(data2012$X.AGEG5YR)
data2012 = data2012[!is.na(data2012$X.AGEG5YR),]
data2012 = subset(data2012, data2012$X.AGEG5YR>2)#removing missing values
data2012 = subset(data2012, data2012$X.AGEG5YR<14) #including for range 1 to 13


#processing for gender
table(data2012$SEX)
data2012 = data2012[!is.na(data2012$SEX),]
table(data2012$SEX)
nrow(data2012)

#processing for ethnicity 
table(data2012$X.RACE.G)
data2012 = data2012[!is.na(data2012$X.RACE.G),]
data2012 = subset(data2012, data2012$X.RACE.G<6)
table(data2012$X.RACE.G)
nrow(data2012)

#processing for BMI
summary(data2012$X.BMI5)
data2012 = data2012[!is.na(data2012$X.BMI5),] #removing missing values
hist(data2013$X.BMI5, main = "Histogram of BMI of respondents",
     xlabe = "BMI",
     ylabe = "number of respondents") #histogram plot for BMI
boxplot(data2012$X.BMI5, main = "Boxplot of BMI")
mean_b = mean(data2012$X.BMI5) #mean of bmi
mean_b
std_b = sd(data2012$X.BMI5) #standard deviation of bmi
std_b
mbl = mean_b-1.96*std_b #lower interval of CI
mbh = mean_b+1.96*std_b #upper interval of CI
mbl
mbh
#CI 95% subset
data2012 = subset(data2012, data2012$X.BMI5 >= mbl) 
data2012 = subset(data2012, data2012$X.BMI5 <= mbh)
data2012$X.BMI5 = (data2012$X.BMI5)/100 #making BMI with 2 significant decimal
summary(data2012$X.BMI5)
nrow(data2012)


#processing for exercise
table(data2012$EXERANY2)
data2012 = data2012[!is.na(data2012$EXERANY2),]
data2012 = subset(data2012, data2012$EXERANY2<7)
table(data2012$EXERANY2)
nrow(data2012)

#processing for mental health
table(data2012$MENTHLTH)
data2012 = data2012[!is.na(data2012$MENTHLTH),]
data2012$MENTHLTH[data2012$MENTHLTH == 88] <- 0
data2012 = subset(data2012, data2012$MENTHLTH<77)
table(data2012$MENTHLTH)
nrow(data2012)

#processing for income
table(data2012$INCOME2)
data2012 = data2012[!is.na(data2012$INCOME2),]
data2012 = subset(data2012, data2012$INCOME2<9)
table(data2012$INCOME2)
nrow(data2012)

#processing for employment
table(data2012$EMPLOY)
data2012 = data2012[!is.na(data2012$EMPLOY),]
data2012$EMPLOY[data2012$EMPLOY ==3] <- 13 
data2012$EMPLOY[data2012$EMPLOY ==5] <- 15
data2012$EMPLOY[data2012$EMPLOY ==4] <- 14
data2012$EMPLOY[data2012$EMPLOY ==6] <- 16
data2012$EMPLOY[data2012$EMPLOY ==13] <- 6 
data2012$EMPLOY[data2012$EMPLOY ==15] <- 3
data2012$EMPLOY[data2012$EMPLOY ==14] <- 5
data2012$EMPLOY[data2012$EMPLOY ==16] <- 4
data2012 = subset(data2012, data2012$EMPLOY<9)
table(data2012$EMPLOY)
nrow(data2012)

#processing for education 
table(data2012$EDUCA)
data2012 = data2012[!is.na(data2012$EDUCA),]
data2012 = subset(data2012, data2012$EDUCA<9)
table(data2012$EDUCA)
nrow(data2012)

#processing  for worried about rent
table(data2012$SCNTMONY)
data2012 = data2012[!is.na(data2012$SCNTMONY),]
data2012 = subset(data2012, data2012$SCNTMONY<6)
table(data2012$SCNTMONY)
nrow(data2012)

#processing for worried about meal
table(data2012$SCNTMEAL)
data2012 = data2012[!is.na(data2012$SCNTMEAL),]
data2012 = subset(data2012, data2012$SCNTMEAL<6)
table(data2012$SCNTMEAL)
nrow(data2012)

#processing for home ownership
table(data2012$RENTHOM1)
data2012 = data2012[!is.na(data2012$RENTHOM1),]
data2012 = subset(data2012, data2012$RENTHOM1<7)
table(data2012$RENTHOM1)
nrow(data2012)

#processing for diabetes
table(data2012$DIABETE3)
data2012 = data2012[!is.na(data2012$DIABETE3),]
data2012 = subset(data2012, data2012$DIABETE3<2)
table(data2012$DIABETE3)
nrow(data2012)
