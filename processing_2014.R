library(SASxport)

data2014 <- read.xport("LLCP2014.XPT")
nrow(data2014)

#Processing Age group
table(data2014$X.AGEG5YR)
data2014 = data2014[!is.na(data2014$X.AGEG5YR),]
data2014 = subset(data2014, data2014$X.AGEG5YR<14)
data2014 = subset(data2014, data2014$DIABAGE2>29)
table(data2014$X.AGEG5YR)
nrow(data2014)

#processing for gender
table(data2014$SEX)
data2014 = data2014[!is.na(data2014$SEX),]
table(data2014$SEX)
nrow(data2014)

#processing for ethnicity 
table(data2014$X.RACEGR3)
data2014 = data2014[!is.na(data2014$X.RACEGR3),]
data2014 = subset(data2014, data2014$X.RACEGR3<9)
table(data2014$X.RACEGR3)
nrow(data2014)

#processing for BMI
summary(data2014$X.BMI5)
data2014 = data2014[!is.na(data2014$X.BMI5),]
data2014 = subset(data2014, data2014$X.BMI5<6000)
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

