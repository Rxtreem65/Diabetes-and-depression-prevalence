#DISTRIBUTION OF DIABETIC RESPONDENTS IN EACH AGE CATEGORES
t = table(mydata$DIABETE3,mydata$X.AGEG5YR)
t
colnames(t) = c("18-23","24-29", "30-34","35-39",
                "40-44","45-49","50-54","55-59",
                "60-64","65-69","70-74","75-79",
                "80+")
barplot(t,
        main="Diabetic distribution of respondents",
        xlab = "age group",
        col=c("brown"),
        #beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Diabetes","Non Diabetes"),
       fill = c("brown","green"),
)

t = round(prop.table(t,margin=2)*100,2)

barplot(t,
        main="Diabetic distribution of respondents",
        xlab = "age group",
        col=c("brown","white"),
        ylab="Percentage of respondents"
)
legend("topleft",
       c("Diabetes"),
       fill = c("brown"),
)

#DISTRIBUTION OF DEPRESSED RESPONDENTS IN EACH AGE CATEGORES
t=table(mydata$ADDEPEV2,mydata$X.AGEG5YR)
colnames(t) = c("18-23","24-29", "30-34","35-39",
                "40-44","45-49","50-54","55-59",
                "60-64","65-69","70-74","75-79",
                "80+")
barplot(t,
        main="Depressed distribution of respondents",
        xlab = "age group",
        col=c("black","white"),
        beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Depressed","Non Depressed"),
       fill = c("black","white"),
)

t = round(prop.table(t,margin=2)*100,2)
barplot(t,
        main="Depressed distribution of respondents",
        xlab = "age group",
        col=c("black","white"),
        #beside = TRUE,
        ylab="Percentage of respondents"
)
legend("topleft",
       c("Depressed"),
       fill = c("black"),
)

