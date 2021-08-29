library(SASxport)

data2013 <- read.xport("C:/Users/riyap/Downloads/data/LLCP2013.XPT")

fea <- c("DIABAGE2","X.AGEG5YR","SEX","X.RACE.G1","X.BMI5","EXERANY2",
         "MENTHLTH","INCOME2","EMPLOY1","EDUCA","SCNTMONY","SCNTMEAL",
         "RENTHOM1","DIABETE3","ADDEPEV2")
data2013 <- data2013[fea]
nrow(data2013)

#processing for diabetes diagnosis age
table(data2013$DIABAGE2)
data2013 = data2013[!is.na(data2013$DIABAGE2),] #removing missing values
hist(data2013$DIABAGE2, 
     main = "Histogram of age at which diabetes diagnosed",
     xlab = "Age at which respondent diagnoised with diabetes",
     ylab = "number of respondents") #histogram plot for diagnosed age
boxplot(data2013$DIABAGE2, main = "Boxplot of age at which diabetes diagnose")
mean_dia = mean(data2013$DIABAGE2) #mean of diabetes age
mean_dia
std_dia = sd(data2013$DIABAGE2) #standard deviation of diabetes age
std_dia
mdl = mean_dia-1.96*std_dia #lower interval of CI
mdh = mean_dia+1.96*std_dia #upper interval of CI
mdl
mdh
#CI 95% subset
data2013 = subset(data2013, data2013$DIABAGE2 >= 30) 
data2013 = subset(data2013, data2013$DIABAGE2 <= mdh)
table(data2013$DIABAGE2)
nrow(data2013)


#Processing Age group
table(data2013$X.AGEG5YR)
data2013 = data2013[!is.na(data2013$X.AGEG5YR),] #removing missing values
data2013 = subset(data2013, data2013$X.AGEG5YR<14) #including for range 1 to 13

#processing for gender
table(data2013$SEX)
data2013 = data2013[!is.na(data2013$SEX),] #removing missing values
table(data2013$SEX) 
nrow(data2013)

#processing for ethnicity 
table(data2013$X.RACE.G1)
data2013 = data2013[!is.na(data2013$X.RACE.G1),] #removing missing values
data2013 = subset(data2013, data2013$X.RACE.G1<6) #including range 1 to 5
table(data2013$X.RACE.G1)
nrow(data2013)

#processing for BMI
summary(data2013$X.BMI5)
data2013 = data2013[!is.na(data2013$X.BMI5),] #removing missing values
hist(data2013$X.BMI5, main = "Histogram of BMI of respondents",
     xlabe = "BMI",
     ylabe = "number of respondents") #histogram plot for BMI
boxplot(data2013$X.BMI5, main = "Boxplot of BMI")
mean_b = mean(data2013$X.BMI5) #mean of bmi
mean_b
std_b = sd(data2013$X.BMI5) #standard deviation of bmi
std_b
mbl = mean_b-1.96*std_b #lower interval of CI
mbh = mean_b+1.96*std_b #upper interval of CI
mbl
mbh
#CI 95% subset
data2013 = subset(data2013, data2013$X.BMI5 >= mbl) 
data2013 = subset(data2013, data2013$X.BMI5 <= mbh)
data2013$X.BMI5 = (data2013$X.BMI5)/100 #making BMI with 2 significant decimal
summary(data2013$X.BMI5)
nrow(data2013)

#processing for exercise
table(data2013$EXERANY2)
data2013 = data2013[!is.na(data2013$EXERANY2),] #removing missing values
data2013 = subset(data2013, data2013$EXERANY2<7) #including only yes or no
table(data2013$EXERANY2)
nrow(data2013)

#processing for mental health
table(data2013$MENTHLTH)
data2013 = data2013[!is.na(data2013$MENTHLTH),] #removing missing values
data2013$MENTHLTH[data2013$MENTHLTH == 88] <- 0 #replacing None with zero
data2013 = subset(data2013, data2013$MENTHLTH<77) #including range 0 to 30
table(data2013$MENTHLTH)
nrow(data2013)

#processing for income
table(data2013$INCOME2)
data2013 = data2013[!is.na(data2013$INCOME2),] #removing missing values
data2013 = subset(data2013, data2013$INCOME2<9) #including range 1 to 8
table(data2013$INCOME2)
nrow(data2013)

#processing for employment
table(data2013$EMPLOY1)
data2013 = data2013[!is.na(data2013$EMPLOY1),] #removing missing values
#changing the order of the employment status
data2013 <- replace(data2013, data2013==3,0) 
data2013 <- replace(data2013, data2013==5,3)
data2013 <- replace(data2013, data2013==4,5)
data2013 <- replace(data2013, data2013==6,4)
data2013 <- replace(data2013, data2013==0,6)
data2013 = subset(data2013, data2013$EMPLOY1<9) #including only 1 to 8
table(data2013$EMPLOY1)
nrow(data2013)

#processing for education 
table(data2013$EDUCA)
data2013 = data2013[!is.na(data2013$EDUCA),] #removing missing values
data2013 = subset(data2013, data2013$EDUCA<9) #including only 1 to 8
table(data2013$EDUCA)
nrow(data2013)

#processing  for worried about rent
table(data2013$SCNTMONY)
data2013 = data2013[!is.na(data2013$SCNTMONY),] #removing missing values
data2013 = subset(data2013, data2013$SCNTMONY<6) #including range 1 to 5
table(data2013$SCNTMONY)
nrow(data2013)

#processing for worried about meal
table(data2013$SCNTMEAL)
data2013 = data2013[!is.na(data2013$SCNTMEAL),] #removing missing values
data2013 = subset(data2013, data2013$SCNTMEAL<6) #including range 1 to 5
table(data2013$SCNTMEAL)
nrow(data2013)

#processing for home ownership
table(data2013$RENTHOM1)
data2013 = data2013[!is.na(data2013$RENTHOM1),] #removing missing values
data2013 = subset(data2013, data2013$RENTHOM1<7) #including rent yes or no
table(data2013$RENTHOM1)
nrow(data2013)

#processing for diabetes
table(data2013$DIABETE3)
data2013 = data2013[!is.na(data2013$DIABETE3),] #removing missing values
data2013 = subset(data2013, data2013$DIABETE3<2) #including diabetes yes or no 
table(data2013$DIABETE3)
nrow(data2013)

#processing for depression
table(data2013$ADDEPEV2)
data2013 = data2013[!is.na(data2013$ADDEPEV2),] #removing missing values
data2013 = subset(data2013, data2013$ADDEPEV2<7) #including depression yes or no
table(data2013$ADDEPEV2)
nrow(data2013)

#Final data
table(data2013$X.AGEG5YR)
table(data2013$DIABAGE2)
table(data2013$SEX)
table(data2013$X.RACE)
summary(data2013$X.BMI5)
table(data2013$EXERANY2)
table(data2013$INCOME2)
table(data2013$EMPLOY)
table(data2013$EDUCA)
table(data2013$SCNTMONY)
table(data2013$SCNTMEAL)
table(data2013$RENTHOM1)
table(data2013$DIABETE3)
table(data2013$ADDEPEV2)
