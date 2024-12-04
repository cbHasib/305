d<-read.csv("C:\\Users\\USER\\Desktop\\2110327126.csv")
D<-as.matrix(d)
y<-D[,1]
x1<-D[,2]
x2<-D[,3]
x3<-D[,4]
x<-cbind(x1,x2,x3)
#for regression 
model<-lm(y~x)
summary(model)
#for bpg test( (heteroskedasticity))
library(lmtest)
lmtest::bgtest(model)
#for white (heteroskedasticity) 
library(skedastic)
skedastic::white(model)
#for run ( autocorrelation)
r=cor(x)
library(lawstat)
rn_test<-runs.test(x)
print(rn_test)
#for Durbin D test( autocorelation) 
library(car)
dw_test<-durbinWatsonTest(model)
print(dw_test)
#determinant of matrix(multicolinearity) 
det_cor_mat<-det(r)
print(det_cor_mat)
#VIF(multicolinearity) 
library(car)
data<-data.frame(y,x1,x2,x3)
model1<-lm(y~x1+x2+x3,data = data)
vif_result<-vif(model1)
print(vif_result)

#transformation of heteroskesticity

library(sandwich)
coeftest(model, vcov = vcovHC(model, type = "HC1"))

# Fit a GLS model with AR(1) correlation structure(autocorrelation)

model_gls <- gls(y ~ x1+ x2+x3, correlation = corAR1(), data = data)
summary(model_gls)

#for multicollinearity
data_scaled <- scale(data[, c("x1", "x2", "x3")])
model_scaled <- lm(y ~ data_scaled, data = data)
summary(model_scaled)