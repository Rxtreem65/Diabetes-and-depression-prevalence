library(SASxport)

data2014 <- read.xport("C:/Users/riyap/Downloads/data/LLCP2014.XPT")
fea <- c("DIABAGE2","X.AGEG5YR","SEX","X.RACE.G1","X.BMI5","EXERANY2",
         "MENTHLTH","INCOME2","EMPLOY1","EDUCA","SCNTMNY1","SCNTMEL1",
         "RENTHOM1","DIABETE3","ADDEPEV2")
data2014 <- data2014[fea]
nrow(data2014)

#processing for diabetes diagnosis age
table(data2014$DIABAGE2)
data2014 = data2014[!is.na(data2014$DIABAGE2),] #removing missing values
hist(data2014$DIABAGE2, 
     main = "Histogram of age at which diabetes diagnosed",
     xlab = "Age at which respondent diagnoised with diabetes",
     ylab = "number of respondents") #histogram plot for diagnosed age
boxplot(data2014$DIABAGE2, main = "Boxplot of age at which diabetes diagnose")
mean_dia = mean(data2014$DIABAGE2) #mean of diabetes age
mean_dia
std_dia = sd(data2014$DIABAGE2) #standard deviation of diabetes age
std_dia
mdl = mean_dia-1.96*std_dia #lower interval of CI
mdh = mean_dia+1.96*std_dia #upper interval of CI
mdl
mdh
#CI 95% subset
data2014 = subset(data2014, data2014$DIABAGE2 >= 30) 
data2014 = subset(data2014, data2014$DIABAGE2 <= mdh)
table(data2014$DIABAGE2)
nrow(data2014)


#Processing Age group
table(data2014$X.AGEG5YR)
data2014 = data2014[!is.na(data2014$X.AGEG5YR),]
data2014 = subset(data2014, data2014$X.AGEG5YR<14)



#processing for gender
table(data2014$SEX)
data2014 = data2014[!is.na(data2014$SEX),]
table(data2014$SEX)
nrow(data2014)

#processing for ethnicity 
table(data2014$X.RACE.G1)
data2014 = data2014[!is.na(data2014$X.RACE.G1),]
data2014 = subset(data2014, data2014$X.RACE.G1<9)
table(data2014$X.RACE.G1)
nrow(data2014)

#processing for BMI
summary(data2014$X.BMI5)
data2014 = data2014[!is.na(data2014$X.BMI5),]#removing missing values
hist(data2014$X.BMI5, main = "Histogram of BMI of respondents",
     xlabe = "BMI",
     ylabe = "number of respondents") #histogram plot for BMI
boxplot(data2014$X.BMI5, main = "Boxplot of BMI")
mean_b = mean(data2014$X.BMI5) #mean of bmi
mean_b
std_b = sd(data2014$X.BMI5) #standard deviation of bmi
std_b
mbl = mean_b-1.96*std_b #lower interval of CI
mbh = mean_b+1.96*std_b #upper interval of CI
mbl
mbh
#CI 95% subset
data2014 = subset(data2014, data2014$X.BMI5 >= mbl) 
data2014 = subset(data2014, data2014$X.BMI5 <= mbh)
data2014$X.BMI5 = (data2014$X.BMI5)/100 #making BMI with 2 significant decimal
summary(data2014$X.BMI5)
nrow(data2014)


#processing for exercise
table(data2014$EXERANY2)
data2014 = data2014[!is.na(data2014$EXERANY2),]
data2014 = subset(data2014, data2014$EXERANY2<7)
table(data2014$EXERANY2)
nrow(data2014)

#processing for mental health
table(data2014$MENTHLTH)
data2014 = data2014[!is.na(data2014$MENTHLTH),]
data2014$MENTHLTH[data2014$MENTHLTH == 88] <- 0
data2014 = subset(data2014, data2014$MENTHLTH<77)
table(data2014$MENTHLTH)
nrow(data2014)

#processing for income
table(data2014$INCOME2)
data2014 = data2014[!is.na(data2014$INCOME2),]
data2014 = subset(data2014, data2014$INCOME2<9)
table(data2014$INCOME2)
nrow(data2014)

#processing for employment
table(data2014$EMPLOY1)
data2014 = data2014[!is.na(data2014$EMPLOY1),]
data2014 = subset(data2014, data2014$EMPLOY1<9)
table(data2014$EMPLOY1)
nrow(data2014)

#processing for education 
table(data2014$EDUCA)
data2014 = data2014[!is.na(data2014$EDUCA),]
data2014 <- replace(data2014, data2014==3,0)
data2014 <- replace(data2014, data2014==5,3)
data2014 <- replace(data2014, data2014==4,5)
data2014 <- replace(data2014, data2014==6,4)
data2014 <- replace(data2014, data2014==0,6)
data2014 = subset(data2014, data2014$EDUCA<9)
table(data2014$EDUCA)
nrow(data2014)

#processing  for worried about rent
table(data2014$SCNTMNY1)
data2014 = data2014[!is.na(data2014$SCNTMNY1),]
data2014 = subset(data2014, data2014$SCNTMNY1<6)
table(data2014$SCNTMNY1)
nrow(data2014)

#processing for worried about meal
table(data2014$SCNTMEL1)
data2014 = data2014[!is.na(data2014$SCNTMEL1),]
data2014 = subset(data2014, data2014$SCNTMEL1<6)
table(data2014$SCNTMEL1)
nrow(data2014)

#processing for home ownership
table(data2014$RENTHOM1)
data2014 = data2014[!is.na(data2014$RENTHOM1),]
data2014 = subset(data2014, data2014$RENTHOM1<7)
table(data2014$RENTHOM1)
nrow(data2014)

#processing for diabetes
table(data2014$DIABETE3)
data2014 = data2014[!is.na(data2014$DIABETE3),]
data2014 = subset(data2014, data2014$DIABETE3<7)
table(data2014$DIABETE3)
nrow(data2014)

#processing for depression
table(data2014$ADDEPEV2)
data2014 = data2014[!is.na(data2014$ADDEPEV2),]
data2014 = subset(data2014, data2014$ADDEPEV2<7)
table(data2014$ADDEPEV2)
nrow(data2014)

