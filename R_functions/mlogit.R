
#  THIS CODE PRODUCE ALL GRAPHS AND RESULTS 
#  DATA SET:
#      VehChoicePrice.csv

#################
#  Example 4
#################

# READ DATA SET
VehownPrice = read.csv("J:\\My Documents\\VehChoicePrice.csv",header=TRUE)

#  CHECK THE NAMES, DIMENSION IN THE FILE AND LIST THE FIRST 10 OBSERVATIONS
names(VehownPrice)
dim(VehownPrice)
VehownPrice[1:10,]
#attach(VehownPrice)
table(VehownPrice$veh)

#  INSTALL THESE PACKAGES ONCE
  install.packages("mlogit")

library(mlogit)

# PREPARE THE DATA
mldata2<-mlogit.data(VehownPrice, varying=5:7, choice="veh", shape="wide")
head(mldata2,5)

# MULTINOMIAL LOGISTIC REGRESSION
mlogit.model<- mlogit(veh~price|men+urban+age, data = mldata2, reflevel="C")
summary(mlogit.model)
