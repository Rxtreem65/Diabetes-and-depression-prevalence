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
                + as.factor(X.RACE.G1) + X.BMI5 + EXERANY2 + MENTHLTH
                + SES , 
                family = binomial,
                data = total_data, )
summary(MORB.total_data)
