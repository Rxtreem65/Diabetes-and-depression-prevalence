#creating new variables SES and morbidity

#SES variable
#higher the value of SES better the social economic status
total_data$SES = total_data$INCOME2+(1/total_data$EMPLOY1)+total_data$EDUCA
              +total_data$SCNTMNY1+total_data$SCNTMEL1+(1/total_data$RENTHOM1)
summary(total_data$SES)

#binomial value for morbidity (0)absent (1)present
total_data$MORB = 2 - total_data$ADDEPEV2 
