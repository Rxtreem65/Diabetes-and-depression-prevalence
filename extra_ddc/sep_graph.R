library(DescTools)
hist(data2015$X.BMI5, xlab="BMI")
hist(data2014$X.BMI5, xlab = "BMI")
hist(data2013$X.BMI5, xlab = "BMI")
hist(data2012$X.BMI5, xlab = "BMI")



#histogram for age at which respondent had diabetes 
hist(data2015$DIABAGE2, xlab="diabetes age")
hist(data2014$DIABAGE2, xlab = "diabetes age")
hist(data2013$DIABAGE2, xlab = "diabetes age")
hist(data2012$DIABAGE2, xlab = "diabetes age")

#centiles value
q = quantile(total_data$SES, c(.0, .10, .25, .50, .75,.90,1))
barplot(q)

par(mar = c(6, 6, 6, 2))
d <- density(total_data$SES)
x = seq(4,25,1)
plot(d, main="Density plot for Socio-economic position of respondents",
     xlab = "Socio-economic position of respondent",
     ylab = "Proportion of respondents",
     cex.lab=1.75, cex.axis=1.5, lwd=4)
#text(x, labels = x, cex=x)
#polygon(c(total_data$SES[total_data$SES>=16],0), col =rgb(0,0,0), lwd=3)
#abline(v = mean(total_data$SES),lwd=3)
abline(v=10, col="black",lwd=4, lty=2)
abline(v=16, col="black",lwd=4, lty=3)

legend( "topleft", 
       legend = c("Low SEP", 
                  "Medium SEP"),
       lty = 2:3, 
       lwd = 3,
       bg="white",
       title="Threshold for:",
       bty = "n", 
       #pt.cex = 6, 
       cex = 1.5,
       box.lty=2, box.lwd=3, box.col = "black",
       text.col = "black", 
       #horiz = F , 
       #inset = c(0.1, 0.1)
       )
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8,
       title="Line types", text.font=4, bg='lightblue')

library(tigerstats)
pnormGC(c(10,16), region = "between")



hist(total_data$X.AGEG5YR)
median(total_data$X.AGEG5YR)
median(total_data$SES)
hist(total_data$SES)
summary(total_data$X.BMI5)
hist(total_data$X.BMI5)

res <- prop.test(x = c(1079, 1133), n = c(9827, 5822))
# Printing the results
res 
res <- prop.test(x = c(1079, 1133), n = c(9827, 5822), alternative = "less")
res
res <- prop.test(x = c(1079, 1133), n = c(9827, 5822), alternative = "greater")
res



MeanCI(total_data$X.AGEG5YR)
MeanCI(total_data$X.BMI5)
MedianCI(total_data$X.BMI5)
MeanCI(total_data$DURATION)
sd(total_data$DURATION)
MeanCI(total_data$SES)
t = table(total_data$MORB_3,total_data$SEX)
t
prop.table(t, margin = 2)
t = table(total_data$MORB_3,total_data$X.RACE.G1)
t
prop.table(t, margin = 2)
t = table(total_data$MORB_3,total_data$EXERANY2)
t
prop.table(t, margin = 2)
t = table(total_data$MORB_3,total_data$SES_category)
t
prop.table(t, margin = 2)


hist(total_data$DURATION,)


hist(total_data$MENTHLTH)
m = mean(x$MENTHLTH)
m
x = subset(total_data, total_data$MENTHLTH>0)

summary(total_data$SES)
hist(total_data$SES)
table(total_data$MORB,total_data$X.AGEG5YR)
m = (mean(total_data$X.AGEG5YR[total_data$MORB==1])-3)*5+32.5
m
m = (mean(total_data$X.AGEG5YR[total_data$MORB==0])-3)*5+32.5
m

f = subset(total_data, total_data$MORB_3==0)
m = subset(total_data, total_data$MORB_3==1)
nrow(m)
MeanCI(m$X.AGEG5YR)
MeanCI(m$X.BMI5)
MeanCI(m$DURATION)
MeanCI(m$SES)
nrow(f)
p = nrow(f)/(nrow(f)+nrow(m))
p

table(total_data$SEX,total_data$MORB_3)
table(f$ADDEPEV2)

table(total_data$ADDEPEV2)
prop.table(table(total_data$ADDEPEV2))

t = prop.table(total_datam$ADDEPEV2,m$MORB_3)
t
f1 = subset(f, f$SES_category==1)
e = mean(f1$X.BMI5)
e
nrow(f1)
f1 = subset(m, m$SEX==2)
nrow(f1)
nrow(m)
nrow(f)/(nrow(f)+nrow(m))

e = mean(m$SES_category)
e
summary(total_data$X.BMI5)
