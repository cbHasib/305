#Data load
D<-read.table("D:/Study/305/Practical/World Bank minimize data.csv",header=TRUE,sep=",")

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

#---------------------------#
#  Heteroscedasticity test  #
#---------------------------#

hetero_plot<-plot(model)

#BPG Test

library(lmtest)
lmtest::bptest(model)

#White Test
library(skedastic) 
skedastic::white(model)



# forecasting of OLS
N<-length(y)
p <- predict(lm(y~x))
e<-resid(model)




#------------------------#
#  Autocorrelation test  #
#------------------------#

library(ggplot2)
autoplot<-plot(model)


#Run test

library(randtests)
run_test_result<-runs.test(e)
print(run_test_result)

#Durbin-Watson d_test

library(car)

dw_test<-durbinWatsonTest(model)
print(dw_test)


#Run test

library(lawstat)
rn_test<-runs.test(e)
print(rn_test)




#--------------------------#
#  Multicollinearity test  #
#--------------------------#



#Examination of Determinant

det_cor_mat<-det(r)
print(det_cor_mat)


#Examination of the correlation matrix

library(corrplot)
corrplot(r)


#Variance inflation factor

library(car)
data<-data.frame(y,x1,x2,x3)
model1<-lm(y~x1+x2+x3,data = data)
vif_result<-vif(model1)
print(vif_result)


#Tolerance

tol_values<-car::vif(model1)
print(tol_value)


#Eigen value decomposition

eigen_decomp<-eigen(r)
print(eigen_decomp)


