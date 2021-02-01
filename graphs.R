library(SASxport)
library(ggplot2)
library(dplyr)

mydata <- read.xport("C:/Users/riyap/R_work/LLCP2019.XPT")

fea <- c("MENTHLTH","ADDEPEV3","CRGVPRB3","DIABETE4",
      "DIABAGE3","PDIABTST","PREDIAB1","INSULIN1","BLDSUGAR",
      "FEETCHK3","DOCTDIAB","CHKHEMO3","FEETCHK","EYEEXAM1",
      "DIABEYE","DIABEDU","CRGVPRB3","SEXVAR","X.AGEG5YR","X.SEX")

#mydata=mydata[,fea]
head(mydata)


#### Data pre-processing ####

# "ADDEPEV3" (Ever told) you had a depressive disorder
# 1 = yes
# 2 = No
table(mydata$ADDEPEV3)
mydata=mydata[!is.na(mydata$ADDEPEV3),]
mydata=subset(mydata,mydata$ADDEPEV3<7)
t = table(mydata$ADDEPEV3)
t
#percent of depressive and non depressive
round(prop.table(t)*100,2)

#"DIABETE4"(Ever told) you had diabetes
# 1 = Yes
# 2 = gestational
# 3 = No
# 4 = borderline
table(mydata$DIABETE4)
mydata=mydata[!is.na(mydata$DIABETE4),]
mydata=subset(mydata, mydata$DIABETE4<7)
mydata=subset(mydata, mydata$DIABETE4!=2)
mydata=subset(mydata, mydata$DIABETE4!=4)
t = table(mydata$DIABETE4)
t
#percent of diabetes and non diabetes
round(prop.table(t)*100,2)

#LANDSEX
#CELLSEX
#Are you male or female?
#SEXVAR = LANDSEX + CELLSEX
#Sex of Respondent
#_SEX = calculated sex
t=table(mydata$X.SEX)
t
#percent of male and female
round(prop.table(t)*100,2)

#AGE
#Fourteen-level age category
table(mydata$X.AGEG5YR)
summary(mydata$X.AGEG5YR)
mydata=subset(mydata,mydata$X.AGEG5YR<14)

dep_data=subset(mydata, mydata$ADDEPEV3!=2)
dep_dia_data=subset(dep_data,dep_data$DIABETE4!=3)

par(mfrow=c(1,1))

#DISTRIBUTION OF RESPONDENTS IN EACH AGE CATEGORIES
barplot(table(mydata$X.AGEG5YR),
        main="Distribution of repodents in each age group",
        xlab="Age group",
        ylab="Number of respondents",
        border="red",
        col="blue",
        density=10
)

table(mydata$X.SEX,mydata$X.AGEG5YR)

#DISTRIBUTION OF MALE AND FEMALE IN EACH AGE CATEGORIES
barplot(table(mydata$X.SEX,mydata$X.AGEG5YR),
        main="Gender distribution of respondents",
        xlab = "age group",
        col=c("red","blue"),
        beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
        c("Male","Female"),
        fill = c("red","blue"),
        )

table(mydata$ADDEPEV3,mydata$X.AGEG5YR)
#DISTRIBUTION OF DEPRESSED RESPONDENTS IN EACH AGE CATEGORES
barplot(table(mydata$ADDEPEV3,mydata$X.AGEG5YR),
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


#distribution of diabetes in each age group
barplot(table(mydata$DIABETE4,mydata$X.AGEG5YR),
        main="Diabetic distribution of respondents",
        xlab = "age group",
        col=c("brown","green"),
        beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Diabetes","Non Diabetes"),
       fill = c("brown","green"),
)

#distribution of depression in diabetes categories
table(mydata$DIABETE4,mydata$ADDEPEV3,mydata$X.AGEG5YR)
barplot(table(mydata$DIABETE4,mydata$ADDEPEV3),
        main="Gender distribution of respondents",
        xlab = "depressed",
        col=c("brown","green"),
        #beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Diabetes","Non Diabetes"),
       fill = c("brown","green"),
)

#distribution of depressed as per gender
barplot(table(mydata$X.SEX,mydata$ADDEPEV3),
        main="Gender distribution of respondents",
        xlab = "depressed",
        col=c("blue","red"),
        beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Male","Female"),
       fill = c("blue","red"),
)

#distribution of diabetes as per gender
barplot(table(mydata$X.SEX,mydata$DIABETE4),
        main="Gender distribution of respondents",
        xlab = "Diabetes",
        col=c("blue","red"),
        beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Male","Female"),
       fill = c("blue","red"),
)

#distribution of diabetes in depression
barplot(table(mydata$ADDEPEV3,mydata$DIABETE4),
        main="Gender distribution of respondents",
        xlab = "Diabetes",
        col=c("black","white"),
        #beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Depressed","Non Depressed"),
       fill = c("black","white"),
)

##
young_data=subset(mydata,mydata$X.AGEG5YR<7)
old_data=subset(mydata,mydata$X.AGEG5YR>6)

barplot(table(young_data$ADDEPEV3,young_data$X.AGEG5YR),
        main="Depressed distribution of respondents in young age group",
        xlab = "age group",
        col=c("black","white"),
        beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Depressed","Non Depressed"),
       fill = c("black","white"),
)

barplot(table(old_data$ADDEPEV3,old_data$X.AGEG5YR),
        main="Depressed distribution of respondents in old age group",
        xlab = "age group",
        col=c("black","white"),
        beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Depressed","Non Depressed"),
       fill = c("black","white"),
)

#distribution of diabetes in depression
barplot(table(old_data$ADDEPEV3,old_data$DIABETE4),
        main="Gender distribution of respondents of old age",
        xlab = "Diabetes",
        col=c("black","white"),
        #beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Depressed","Non Depressed"),
       fill = c("black","white"),
)

barplot(table(old_data$DIABETE4,old_data$ADDEPEV3),
        main="depressed distribution of respondents in old age",
        xlab = "depressed",
        col=c("brown","green"),
        #beside = TRUE,
        ylab="Number of respondents"
)
legend("topleft",
       c("Diabetes","Non Diabetes"),
       fill = c("brown","green"),
)

ggplot(data = mydata, mapping = aes(x=X.AGEG5YR,y=ADDEPEV3, fill=ADDEPEV3)) +
  geom_bar(stat="identity", position = "dodge")

#plot for age group vs depression
ggplot(data=mydata, aes(x=X.AGEG5YR,y=ADDEPEV3,fill=ADDEPEV3)) +
  geom_col()
  #geom_bar(stat="identity")

#plot for age group vs depression with gender
ggplot(dep_data, aes(x=X.AGEG5YR,y=ADDEPEV3,fill=X.SEX )) +
  geom_bar(stat="identity")

#plot for age group vs depression with diabetes
ggplot(dep_dia_data, aes(x=X.AGEG5YR,y=ADDEPEV3,fill=X.SEX)) +
  geom_bar(stat="identity")
                


