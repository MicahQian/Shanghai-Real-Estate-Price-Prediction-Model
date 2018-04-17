#read the data
X=read.csv("F:\\data.csv") 

#draw scatterplot of data
attach(X) 
plot(t,y)

#transform a non-linear model (exponential model) to a linear regression model 
y1=log(y)
a1=log(t)

#run the linear regression for two variables
reg=lm(y1~t)
summary(reg) #see outcome

#use the Intercept and coefficient gived above to transform a linear regression model back to anon-linear model (exponential model)
a1= -3.284e+02
a=exp(a1)
b= 1.681e-01 
yy=a*exp(b*t)

#draw scatterplot of data and line the non-linear model on the scatterplot
plot(t,y)
lines(t,yy)

plot(X)

#do Correlation test for all variables
cor(X)
attach(X)

#do Correlation test for each variable
cor.test(y,x1)  
cor.test(y,x2)  
cor.test(y,x3)  
cor.test(y,x4)  
cor.test(y,x5) 
cor.test(y,x6)    
cor.test(y,x7) 

#run the linear regression for all variables
reg1=lm(y~x1+x2+x3+x4+x5+x6+x7)
summary(reg1)

#remove x5 and run the linear regression for 6 variables: second attempt
reg2=lm(y~x1+x2+x3+x4+x6+x7)
summary(reg2)

#remove x4, x5 and run the linear regression for 5 variables: third attempt
reg3=lm(y~x1+x2+x3+x6+x7)
summary(reg3)

#remove x4, x5, x6 and run the linear regression for 4 variables: fourth attempt
reg4=lm(y~x1+x2+x3+x6)
summary(reg4)
