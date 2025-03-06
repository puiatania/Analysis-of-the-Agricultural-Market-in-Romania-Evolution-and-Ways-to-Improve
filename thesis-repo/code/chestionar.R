library(readxl)
library(moments)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(openxlsx)
library(ca)
chestionar <- read_excel("chestionar.xlsx")
View(chestionar)
dim(chestionar)

#pentru varsta 
contingenta<-table(chestionar$X1,chestionar$X16)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#pentru ocupatie
contingenta<-table(chestionar$X2,chestionar$X18)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#pentru venit
contingenta<-table(chestionar$X3,chestionar$X18)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#pentru copii in intretinere
contingenta<-table(chestionar$X7,chestionar$X31)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X4,chestionar$X22)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X38,chestionar$X45)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X42,chestionar$X48)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X50,chestionar$X57)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X17,chestionar$X23)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X22,chestionar$X20)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X26,chestionar$X27)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X35,chestionar$X53)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X30,chestionar$X31)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X11,chestionar$X48)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X23,chestionar$X24)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X28,chestionar$X30)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X4,chestionar$X26)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X4,chestionar$X38)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X4,chestionar$X40)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

#
contingenta<-table(chestionar$X4,chestionar$X42)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

contingenta<-table(chestionar$X16,chestionar$X22)
contingenta
X2<-chisq.test(contingenta)
X2
X2$observed
round(X2$expected,2)
ac<-ca(contingenta)
plot(ac)
summary(ac)

