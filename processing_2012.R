library(SASxport)

data2012 <- read.xport("C:/Users/riyap/Downloads/data/LLCP2012.XPT")
nrow(data2012)

#Processing Age group
table(data2012$X.AGEG5YR)
table(data2012$DIABAGE2)
data2012 = data2012[!is.na(data2012$X.AGEG5YR),]
data2012 = subset(data2012, data2012$X.AGEG5YR<14)
data2012 = subset(data2012, data2012$DIABAGE2>29)
table(data2012$X.AGEG5YR)
nrow(data2012)

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
data2012 = data2012[!is.na(data2012$X.BMI5),]
data2013 = subset(data2013, data2013$X.BMI5<6000)
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
data2012$EMPLOY[data2012$EMPLOY ==3] <- 0 
data2012$EMPLOY[data2012$EMPLOY ==5] <- 3
data2012$EMPLOY[data2012$EMPLOY ==4] <- 5
data2012$EMPLOY[data2012$EMPLOY ==6] <- 4
data2012$EMPLOY[data2012$EMPLOY ==0] <- 6
data2012 = subset(data2012, data2012$EMPLOY<9)
table(data2012$EMPLOY)
nrow(data2012)

#processing for education 
table(data2012$EDUCA)
data2012 = data2015[!is.na(data2012$EDUCA),]
data2012 = subset(data2012, data2012$EDUCA<9)
table(data2012$EDUCA)
nrow(data2012)

#processing  for worried about rent
table(data2012$SCNTMNY1)
data2012 = data2012[!is.na(data2012$SCNTMNY1),]
data2012 = subset(data2012, data2012$SCNTMNY1<6)
table(data2012$SCNTMNY1)
nrow(data2012)

#processing for worried about meal
table(data2012$SCNTMEL1)
data2012 = data2012[!is.na(data2012$SCNTMEL1),]
data2012 = subset(data2012, data2015$SCNTMEL1<6)
table(data2012$SCNTMEL1)
nrow(data2012)

#processing for home ownership
table(data2012$RENTHOM1)
data2012 = data2015[!is.na(data2012$RENTHOM1),]
data2012 = subset(data2012, data2012$RENTHOM1<7)
table(data2012$RENTHOM1)
nrow(data2012)

#processing for diabetes
table(data2012$DIABETE3)
data2012 = data2012[!is.na(data2012$DIABETE3),]
data2012 = subset(data2012, data2012$DIABETE3<2)
table(data2012$DIABETE3)
nrow(data2012)

#processing for depression
table(data2012$ADDEPEV2)
data2012 = data2012[!is.na(data2012$ADDEPEV2),]
data2012 = subset(data2012, data2012$ADDEPEV2<7)
table(data2012$ADDEPEV2)
nrow(data2012)
