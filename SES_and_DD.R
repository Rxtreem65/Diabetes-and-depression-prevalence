#creating new variables SES and morbidity

#SES variable
#higher the value of SES better the social economic status
total_data$SES = total_data$INCOME2+(1/total_data$EMPLOY1)+total_data$EDUCA +total_data$SCNTMNY1+total_data$SCNTMEL1+(1/total_data$RENTHOM1)
summary(total_data$SES)

#binomial value for morbidity (0)absent (1)present
total_data$MORB = 2 - total_data$ADDEPEV2 

table(total_data$SES)
table(total_data$ADDEPEV2)
hist(total_data$SES)
hist(total_data$SES,
     xlab = "Social economic Status", 
     ylab = "Percentage of Respondents", 
     freq = FALSE, 
     xlim = c(2,15),
     main = "Variation of SES among Respondents"
     )
axis(side=1, at=seq(2,15, 1), labels=seq(2,15,1))

hist(total_data$MENTHLTH,xlab = "Poor Mental Health Days", 
     ylab = "Percentage of Respondents", 
     main = "Variation of poor mental health among Respondents")

hist(total_data$X.BMI5,xlab = "BMI", 
     ylab = "Percentage of Respondents", 
     main = "Variation of BMI among Respondents",
     xlim = c(16,50),
     freq = FALSE,
     ylim = c(0,0.08)
     )
summary(total_data$X.BMI5)

boxplot(total_data$SES)
