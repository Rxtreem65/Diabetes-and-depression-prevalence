library(DescTools)

total_data <- read.xport("tot_data.xpt")

MeanCI(total_data$X.BMI5)
MedianCI(total_data$X.BMI5)
MeanCI(total_data$DURATION)
sd(total_data$DURATION)
MeanCI(total_data$SES)
t = table(total_data$MORB,total_data$SEX)
t
prop.table(t, margin = 2)
t = table(total_data$MORB,total_data$X.RACE.G1)
t
prop.table(t, margin = 2)
t = table(total_data$MORB,total_data$EXERANY2)
t
prop.table(t, margin = 2)
t = table(total_data$MORB,total_data$SES_category)
t
prop.table(t, margin = 2)


hist(total_data$DURATION,)
hist(total_data$MENTHLTH)
summary(total_data$X.BMI5)


mtp_total_data = total_data
MeanCI(mtp_total_data$X.BMI5)
MeanCI(mtp_total_data$MENTHLTH)
MeanCI(mtp_total_data$SES)

t = table(mtp_total_data$MORB,mtp_total_data$SEX)
t
prop.table(t, margin = 2)
t = table(mtp_total_data$MORB,mtp_total_data$X.RACE.G1)
t
prop.table(t, margin = 2)
t = table(mtp_total_data$MORB,mtp_total_data$EXERANY2)
t
prop.table(t, margin = 2)
t = table(mtp_total_data$MORB,mtp_total_data$SES_category)
t
prop.table(t, margin = 2)




######### propotionality test #########


# proportionality test for gender
res <- prop.test(x = c(1436 , 776), n = c(8424, 7225))
res

# proportionality test for exercise 
res <- prop.test(x = c(1079, 1133), n = c(9827, 5822))
res

