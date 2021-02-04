library(SASxport)

data2013 <- read.xport("LLCP2013.XPT")
nrow(data2013)

#Processing Age group
table(data2013$X.AGEG5YR)
table(data2013$DIABAGE2)
data2013 = data2013[!is.na(data2013$X.AGEG5YR),]
data2013 = subset(data2013, data2013$X.AGEG5YR<14)
data2013 = subset(data2013, data2013$DIABAGE2>29)
table(data2013$X.AGEG5YR)
nrow(data2013)

#processing for gender
table(data2013$SEX)
data2013 = data2013[!is.na(data2013$SEX),]
table(data2013$SEX)
nrow(data2013)

#processing for ethnicity 
table(data2013$X.RACE.G)
data2013 = data2013[!is.na(data2013$X.RACE.G),]
data2013 = subset(data2013, data2013$X.RACE.G<6)
table(data2013$X.RACE.G)
nrow(data2013)

#processing for BMI
summary(data2013$X.BMI5)
data2013 = data2013[!is.na(data2013$X.BMI5),]
data2013 = subset(data2013, data2013$X.BMI5<6000)
summary(data2013$X.BMI5)
nrow(data2013)

#processing for exercise
table(data2013$EXERANY2)
data2013 = data2013[!is.na(data2013$EXERANY2),]
data2013 = subset(data2013, data2013$EXERANY2<7)
table(data2013$EXERANY2)
nrow(data2013)

#processing for mental health
table(data2013$MENTHLTH)
data2013 = data2013[!is.na(data2013$MENTHLTH),]
data2013$MENTHLTH[data2013$MENTHLTH == 88] <- 0
data2013 = subset(data2013, data2013$MENTHLTH<77)
table(data2013$MENTHLTH)
nrow(data2013)

#processing for income
table(data2013$INCOME2)
data2013 = data2013[!is.na(data2013$INCOME2),]
data2013 = subset(data2013, data2013$INCOME2<9)
table(data2013$INCOME2)
nrow(data2013)

#processing for employment
table(data2013$EMPLOY1)
data2013 = data2013[!is.na(data2013$EMPLOY1),]
data2013 <- replace(data2013, data2013==3,0)
data2013 <- replace(data2013, data2013==5,3)
data2013 <- replace(data2013, data2013==4,5)
data2013 <- replace(data2013, data2013==6,4)
data2013 <- replace(data2013, data2013==0,6)
data2013 = subset(data2013, data2013$EMPLOY1<9)
table(data2013$EMPLOY1)
nrow(data2013)

#processing for education 
table(data2013$EDUCA)
data2013 = data2013[!is.na(data2013$EDUCA),]
data2013 = subset(data2013, data2013$EDUCA<9)
table(data2013$EDUCA)
nrow(data2013)

#processing  for worried about rent
table(data2013$SCNTMONY)
data2013 = data2013[!is.na(data2013$SCNTMONY),]
data2013 = subset(data2013, data2013$SCNTMONY<6)
table(data2013$SCNTMONY)
nrow(data2013)

#processing for worried about meal
table(data2013$SCNTMEAL)
data2013 = data2013[!is.na(data2013$SCNTMEAL),]
data2013 = subset(data2013, data2013$SCNTMEAL<6)
table(data2013$SCNTMEAL)
nrow(data2013)

#processing for home ownership
table(data2013$RENTHOM1)
data2013 = data2013[!is.na(data2013$RENTHOM1),]
data2013 = subset(data2013, data2013$RENTHOM1<7)
table(data2013$RENTHOM1)
nrow(data2013)

#processing for diabetes
table(data2013$DIABETE3)
data2013 = data2013[!is.na(data2013$DIABETE3),]
data2013 = subset(data2013, data2013$DIABETE3<2)
table(data2013$DIABETE3)
nrow(data2013)

#processing for depression
table(data2013$ADDEPEV2)
data2013 = data2013[!is.na(data2013$ADDEPEV2),]
data2013 = subset(data2013, data2013$ADDEPEV2<7)
table(data2013$ADDEPEV2)
nrow(data2013)
