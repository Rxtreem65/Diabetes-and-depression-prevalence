#creating new variables SES and morbidity

#SES variable
#higher the value of SES better the social economic status
total_data$SES = total_data$INCOME2+(1/total_data$EMPLOY1)+total_data$EDUCA+total_data$SCNTMNY1+total_data$SCNTMEL1+(1/total_data$RENTHOM1)
summary(total_data$SES)
hist(total_data$SES)

# creating SEP categories
total_data$SES_category = 0
total_data$SES_category[total_data$SES <= 10] = 1
total_data$SES_category[total_data$SES > 10 & total_data$SES<= 16] = 2
total_data$SES_category[total_data$SES >16] = 3
table(total_data$SES_category)

#binomial value for morbidity (0) absent (1) present
total_data$MORB = 0
total_data$MORB[total_data$MENTHLTH>13] = 1

#binomial value for performing exercise (0) absent (1) present
total_data$EXERANY2[total_data$EXERANY2==2] <- 0


# creating variable for duration 
#total_data$DURATION = total_data$X.AGEG5YR
total_data$DURATION[total_data$DIABAGE2<35] = 3
total_data$DURATION[total_data$DIABAGE2>=35 & total_data$DIABAGE2<40] = 4
total_data$DURATION[total_data$DIABAGE2>=40 & total_data$DIABAGE2<45] = 5
total_data$DURATION[total_data$DIABAGE2>=45 & total_data$DIABAGE2<50] = 6
total_data$DURATION[total_data$DIABAGE2>=50 & total_data$DIABAGE2<55] = 7
total_data$DURATION[total_data$DIABAGE2>=55 & total_data$DIABAGE2<60] = 8
total_data$DURATION[total_data$DIABAGE2>=60 & total_data$DIABAGE2<65] = 9
total_data$DURATION[total_data$DIABAGE2>=65 & total_data$DIABAGE2<70] = 10
total_data$DURATION[total_data$DIABAGE2>=70 & total_data$DIABAGE2<75] = 11
total_data$DURATION[total_data$DIABAGE2>=75 & total_data$DIABAGE2<80] = 12
total_data$DURATION[total_data$DIABAGE2>=80] = 13
total_data$DURATION = total_data$X.AGEG5YR - total_data$DURATION
total_data$DURATION = total_data$DURATION + 1
total_data = subset(total_data, total_data$DURATION>=0,)

table(total_data$DURATION)
mean(total_data$DURATION)
hist(total_data$DURATION)
mean(total_data$SES)

write.xport(total_data, file = "~/Documents/DDC_study/tot_data.xpt")


