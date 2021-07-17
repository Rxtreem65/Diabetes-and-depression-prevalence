#Processing Age group
table(data201$X.AGEG5YR)
table(data201$DIABAGE2)
data201 = data201[!is.na(data2013$X.AGEG5YR),]
data201 = subset(data2013, data2013$X.AGEG5YR<14)
data201 = subset(data201, data201$DIABAGE2>29)
data201 = subset(data201, data201$DIABAGE2<=80)
table(data2013$X.AGEG5YR)
nrow(data2013)
table(data201$X.BMI5)
p= nrow(data55)/nrow(data201)
p

data55 = subset(data201, data201$X.BMI5>5500)
table(data55$X.BMI5)

data201 = data201[!is.na(data201$X.BMI5),]

hist(data201$DIABAGE2, xlab = "diabetes age")
hist(data201$X.BMI5, xlab = "BMI")
BM= mean(data201$X.BMI5)
BM
bm_std = sd(data201$X.BMI5)
bml=BM-1.96*bm_std
bml
bmh
bmh=BM + 1.96*bm_std
abline(v=bml, col="blue")
abline(v=bmh, col="blue")
summary(data201$X.BMI5)
data201[data201$X.BMI5==1253,"DIABAGE2"]

abline(v=14)
abline(v=89)
abline(v=52)
abline(v=42,col="red")
abline(v=ml, col="blue")
abline(v=mh, col="blue")
m = mean(data201$DIABAGE2)
m
ml= m-1.96*std
mh= m+1.96*std
ml
mh
std = sd(data201$DIABAGE2)
std
bp = boxplot(data201$DIABAGE2)
bp$stats
