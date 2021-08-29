################################################################################

# pre-processing variables of year 2015

################################################################################


library(SASxport)

data2015 <- read.xport("LLCP2015.XPT")
#features to include in the model
fea <- c("DIABAGE2","X.AGEG5YR","SEX","X.RACE.G1","X.BMI5","EXERANY2",
         "MENTHLTH","INCOME2","EMPLOY1","EDUCA","SCNTMNY1","SCNTMEL1",
         "RENTHOM1","DIABETE3","ADDEPEV2")
data2015 <- data2015[fea]
nrow(data2015)

#processing for diabetes diagnosis age
table(data2015$DIABAGE2)
data2015 = data2015[!is.na(data2015$DIABAGE2),] #removing missing values
hist(data2015$DIABAGE2, 
     main = "Histogram of age at which diabetes diagnosed",
     xlab = "Age at which respondent diagnoised with diabetes",
     ylab = "number of respondents") #histogram plot for diagnosed age
boxplot(data2015$DIABAGE2, main = "Boxplot of age at which diabetes diagnose")
mean_dia = mean(data2015$DIABAGE2) #mean of diabetes age
mean_dia
std_dia = sd(data2015$DIABAGE2) #standard deviation of diabetes age
std_dia
mdl = mean_dia-1.96*std_dia #lower interval of CI
mdh = mean_dia+1.96*std_dia #upper interval of CI
mdl
mdh
#CI 95% subset
data2015 = subset(data2015, data2015$DIABAGE2 >= 30) 
data2015 = subset(data2015, data2015$DIABAGE2 <= mdh)
table(data2015$DIABAGE2)
nrow(data2015)


#Processing Age group
table(data2015$X.AGEG5YR)
data2015 = data2015[!is.na(data2015$X.AGEG5YR),]
data2015 = subset(data2015, data2015$X.AGEG5YR>2)
data2015 = subset(data2015, data2015$X.AGEG5YR<14)


#processing for gender
table(data2015$SEX)
data2015 = data2015[!is.na(data2015$SEX),]
table(data2015$SEX)
nrow(data2015)

#processing for ethnicity 
table(data2015$X.RACE.G1)
data2015 = data2015[!is.na(data2015$X.RACE.G1),]
data2015 = subset(data2015, data2015$X.RACE.G1<6)
table(data2015$X.RACE.G1)
nrow(data2015)

#processing for BMI
summary(data2015$X.BMI5)
data2015 = data2015[!is.na(data2015$X.BMI5),]#removing missing values
hist(data2015$X.BMI5, main = "Histogram of BMI of respondents",
     xlabe = "BMI",
     ylabe = "number of respondents") #histogram plot for BMI
boxplot(data2015$X.BMI5, main = "Boxplot of BMI")
mean_b = mean(data2015$X.BMI5) #mean of bmi
mean_b
std_b = sd(data2015$X.BMI5) #standard deviation of bmi
std_b
mbl = mean_b-1.96*std_b #lower interval of CI
mbh = mean_b+1.96*std_b #upper interval of CI
mbl
mbh
#CI 95% subset
data2015 = subset(data2015, data2015$X.BMI5 >= mbl) 
data2015 = subset(data2015, data2015$X.BMI5 <= mbh)
data2015$X.BMI5 = (data2015$X.BMI5)/100 #making BMI with 2 significant decimal
summary(data2015$X.BMI5)
nrow(data2015)

#processing for exercise
table(data2015$EXERANY2)
data2015 = data2015[!is.na(data2015$EXERANY2),]
data2015 = subset(data2015, data2015$EXERANY2<7)
table(data2015$EXERANY2)
nrow(data2015)

#processing for mental health
table(data2015$MENTHLTH)
data2015 = data2015[!is.na(data2015$MENTHLTH),]
data2015$MENTHLTH[data2015$MENTHLTH == 88] <- 0
data2015 = subset(data2015, data2015$MENTHLTH<77)
table(data2015$MENTHLTH)
nrow(data2015)

#processing for income
table(data2015$INCOME2)
data2015 = data2015[!is.na(data2015$INCOME2),]
data2015 = subset(data2015, data2015$INCOME2<9)
table(data2015$INCOME2)
nrow(data2015)

#processing for employment
table(data2015$EMPLOY1)
data2015 = data2015[!is.na(data2015$EMPLOY1),]
data2015$EMPLOY1[data2015$EMPLOY1 ==3] <- 13 
data2015$EMPLOY1[data2015$EMPLOY1 ==5] <- 15
data2015$EMPLOY1[data2015$EMPLOY1 ==4] <- 14
data2015$EMPLOY1[data2015$EMPLOY1 ==6] <- 16
data2015$EMPLOY1[data2015$EMPLOY1 ==13] <- 6 
data2015$EMPLOY1[data2015$EMPLOY1 ==15] <- 3
data2015$EMPLOY1[data2015$EMPLOY1 ==14] <- 5
data2015$EMPLOY1[data2015$EMPLOY1 ==16] <- 4
data2015 = subset(data2015, data2015$EMPLOY1<9)
table(data2015$EMPLOY1)
nrow(data2015)

#processing for education 
table(data2015$EDUCA)
data2015 = data2015[!is.na(data2015$EDUCA),]
data2015 = subset(data2015, data2015$EDUCA<9)
table(data2015$EDUCA)
nrow(data2015)

#processing  for worried about rent
table(data2015$SCNTMNY1)
data2015 = data2015[!is.na(data2015$SCNTMNY1),]
data2015 = subset(data2015, data2015$SCNTMNY1<6)
table(data2015$SCNTMNY1)
nrow(data2015)

#processing for worried about meal
table(data2015$SCNTMEL1)
data2015 = data2015[!is.na(data2015$SCNTMEL1),]
data2015 = subset(data2015, data2015$SCNTMEL1<6)
table(data2015$SCNTMEL1)
nrow(data2015)

#processing for home ownership
table(data2015$RENTHOM1)
data2015 = data2015[!is.na(data2015$RENTHOM1),]
data2015 = subset(data2015, data2015$RENTHOM1<7)
table(data2015$RENTHOM1)
nrow(data2015)

#processing for diabetes
table(data2015$DIABETE3)
data2015 = data2015[!is.na(data2015$DIABETE3),]
data2015 = subset(data2015, data2015$DIABETE3<2)
table(data2015$DIABETE3)
nrow(data2015)