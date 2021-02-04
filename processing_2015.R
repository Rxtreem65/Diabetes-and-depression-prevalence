library(SASxport)

data2015 <- read.xport("C:/Users/riyap/Downloads/data/LLCP2015.XPT")
nrow(data2015)

#Processing Age group
table(data2015$X.AGEG5YR)
table(data2015$DIABAGE2)
data2015 = data2015[!is.na(data2015$X.AGEG5YR),]
data2015 = subset(data2015, data2015$X.AGEG5YR<14)
data2015 = subset(data2015, data2015$DIABAGE2>29)
table(data2015$X.AGEG5YR)
nrow(data2015)

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
data2015 = data2015[!is.na(data2015$X.BMI5),]
data2015 = subset(data2015, data2015$X.BMI5<6000)
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
data2015 <- replace(data2015, data2015==3,0)
data2015 <- replace(data2015, data2015==5,3)
data2015 <- replace(data2015, data2015==4,5)
data2015 <- replace(data2015, data2015==6,4)
data2015 <- replace(data2015, data2015==0,6)
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

#processing for depression
table(data2015$ADDEPEV2)
data2015 = data2015[!is.na(data2015$ADDEPEV2),]
data2015 = subset(data2015, data2015$ADDEPEV2<7)
table(data2015$ADDEPEV2)
nrow(data2015)

#Final data
table(data2015$X.AGEG5YR)
table(data2015$DIABAGE2)
table(data2015$SEX)
table(data2015$X.RACE.G1)
summary(data2015$X.BMI5)
summary(data2015$MENTHLTH)
table(data2015$EXERANY2)
table(data2015$INCOME2)
table(data2015$EMPLOY1)
table(data2015$EDUCA)
table(data2015$SCNTMNY1)
table(data2015$SCNTMEL1)
table(data2015$RENTHOM1)
table(data2015$DIABETE3)
table(data2015$ADDEPEV2)
