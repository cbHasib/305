               
#---------------------------------------------------------------------
#
#  26 March 2023 (Regression: Restricted LS + Heteroscedasticity)
#
#-------------------------------------------------------------------#


#Data load
D<-read.table("D:/Practical/Data20230326.csv",header=T,sep=",")

#matrix formation

data<-as.matrix(D)


#Dependent variable
 
y<-data[,2]

#Independent Variables

x1<-data[,3]
x2<-data[,4]
x3<-data[,5]


# X matrix

x<-cbind(x1,x2,x3)


#OLS regression  

model<-lm(y~x)
summary(model)

#Correlation Matrix

r=cor(x)

#--------------------
#  Heteroscedasticity test
#------------------------#

hetero_plot<-plot(model)

#BPG Test

library(lmtest)
lmtest::bptest(model)

#White Test
library(skedastic) # install.packages("skedastic")
skedastic::white(model)



# forecasting of OLS
N<-length(y)
p <- predict(lm(y~x))
e<-resid(model)




