
total_data <- read.xport("tot_data.xpt")

#creating model

class(total_data$X.AGEG5YR)
class(total_data$SEX)
class(total_data$X.RACE.G1)
class(total_data$X.BMI5)
class(total_data$EXERANY2)
class(total_data$MENTHLTH)
class(total_data$SES)


#MODEL
MORB.total_data = glm(formula = MORB ~ X.AGEG5YR + as.factor(SEX)
                        + as.factor(X.RACE.G1) + X.BMI5 + as.factor(EXERANY2) 
                        + SES + DURATION , 
                        family = binomial,
                        data = total_data, )
summary(MORB.total_data)
exp(coef(MORB.total_data))
exp(confint(MORB.total_data))

MORB_2.total_data = glm(formula = MORB ~ X.AGEG5YR + as.factor(SEX)
                        + as.factor(X.RACE.G1) + X.BMI5 + as.factor(EXERANY2) 
                        + as.factor(SES_category)+ DURATION, 
                        family = binomial,
                        data = total_data, )
summary(MORB_2.total_data)
exp(coef(MORB_2.total_data))
exp(confint(MORB_2.total_data))


# trying interaction term

MORB_5.total_data = glm(formula = MORB ~ X.AGEG5YR + as.factor(SEX)
                        + as.factor(X.RACE.G1) + X.BMI5 + as.factor(EXERANY2) 
                        + SES + DURATION + SES*DURATION, 
                        family = binomial,
                        data = total_data, )
summary(MORB_5.total_data)
exp(coef(MORB_5.total_data))
confint(MORB_5.total_data)
exp(confint(MORB_5.total_data))

MORB_6.total_data = glm(formula = MORB ~ X.AGEG5YR + as.factor(SEX)
                        + as.factor(X.RACE.G1) + X.BMI5 + as.factor(EXERANY2) 
                        + as.factor(SES_category)+ DURATION 
                        + as.factor(SES_category)*DURATION, 
                        family = binomial,
                        data = total_data, )
summary(MORB_6.total_data)
exp(coef(MORB_6.total_data))
exp(confint(MORB_6.total_data))

MORB_7.total_data = glm(formula = MORB ~ X.AGEG5YR + as.factor(SEX)
                        + as.factor(X.RACE.G1) + X.BMI5 + as.factor(EXERANY2) 
                        + SES + DURATION + SES*DURATION + X.AGEG5YR*DURATION, 
                        family = binomial,
                        data = total_data, )
summary(MORB_7.total_data)
exp(coef(MORB_7.total_data))
exp(confint(MORB_7.total_data))


# studying how SEP fares with it's constituent elements
SES.total_data = lm(formula = SES ~ INCOME2 + EMPLOY1 + EDUCA 
                    + SCNTMNY1 + SCNTMEL1 + RENTHOM1 ,
                    data = total_data)
summary(SES.total_data)

SES_2.total_data = lm(formula = SES ~ INCOME2 + EDUCA,
                      data = total_data)
summary(SES_2.total_data)

