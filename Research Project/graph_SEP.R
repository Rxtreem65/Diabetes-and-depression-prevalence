library(DescTools)

total_data <- read.xport("tot_data.xpt")
#par(mar = c(6, 6, 6, 2))
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

