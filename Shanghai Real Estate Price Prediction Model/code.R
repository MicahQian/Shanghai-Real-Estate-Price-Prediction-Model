#read the train data
X=read.csv("F:\\data.csv") 
#read the test data
Xt=read.csv("F:\\data2.csv") 

#draw scatterplot of data
attach(X) 
plot(t,y,xlab='time',ylab='price',main ='Figure 1: The scatterplot of Shanghai Real Estate histrionic price' )

##library(ggplot2)
#ggplot(X, aes(t,y)) + geom_point(color="black",size=3)+ theme_bw() + xlab("time")+ ylab("price") + 
# ggtitle("Figure 1: The scatterplot of Shanghai Real Estate histrionic price")

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

yydata=data.frame(t=X$t,y1=X$y,y2=as.integer(yy))

#draw scatterplot of data and line the non-linear model on the scatterplot
plot(t,y,main = 'scatterplot of data and line the non-linear model on the scatterplot', sub = NULL, xlab = 'time', ylab = 'price')
lines(t,yy)

ggplot(yydata, aes(t,y2)) + geom_point(color="black",size=3)+ geom_line()+ theme_bw() + xlab("time")+ ylab("price") + 
  ggtitle("Figure 3: The scatterplot of Shanghai Real Estate histrionic price")


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

#run stepwise regression for each variable
reg5=step(lm(y~x1+x2+x3+x4+x5+x6+x7),direction="both")
summary(reg5)

#run backward regression for each variable
reg6=step(lm(y~x1+x2+x3+x4+x5+x6+x7),direction="backward")
summary(reg6)

#read the test data
Xt=read.csv("F:\\data2.csv") 

#make prediction for Regression 1 in next three years
pred1=data.frame(t=Xt$t)
reg.pred=predict(reg,pred1,interval="prediction",level=0.95)
y1pred=exp(reg.pred)
y1pred

#use Regression 5 to caculte price for pervious year
yreg5=data.frame(x1=X$x1,x2=X$x2,x3=X$x3,x6=X$x6)
yreg5.pred=predict(reg5,yreg5,interval="prediction",level=0.95)
yreg5.pred
y5=c(yreg5.pred[,1])

plot(t,y,xlab='time',ylab='price',pch=19,cex=1.5,col='red',main ='Figure 3: The scatterplot of Shanghai Real Estate histrionic price')
lines(t,yy,col='green',lwd=2)
lines(t,y5,col='blue',lwd=2)


#make predictions for Regression 5 in next three years
pred5=data.frame(x1=Xt$x1,x2=Xt$x2,x3=Xt$x3,x6=Xt$x6)
reg5.pred=predict(reg5,pred5,interval="prediction",level=0.95)
reg5.pred

#make predictions for Regression 6 in next three years
pred6=data.frame(x1=Xt$x1,x2=Xt$x2,x3=Xt$x3,x6=Xt$x6)
reg6.pred=predict(reg6,pred6,interval="prediction",level=0.95)
reg6.pred

library(psych)
#predition analysis for Regression 1
errorrate1= abs((y1pred[,1]-Xt$y)/Xt$y)*100
predanre1=data.frame(Xt=as.integer(Xt$t),Xt$y,y1pred,errorrate1)
predanre1
describe(errorrate1,type = 2)

#ggplot(predanre1, aes(errorrate1)) + geom_bar(aes(predanre1$errorrate1,predanre1$Xt.t))+ theme_grey() + ylim(0,10)+ xlab("time")+ ylab("error rate") + 
ggtitle("Figure 3: The error rate of non-linear prediction model")

#predition analysis for Regression 4
#errorrate4= abs((reg4.pred[,1]-Xt$y)/Xt$y)*100
#predanre4=data.frame(Xt$t,Xt$y,reg4.pred,errorrate4)
#predanre4
#describe(errorrate4,type = 2)

#predition analysis for Regression 5
errorrate5= abs((reg5.pred[,1]-Xt$y)/Xt$y)*100
predanre5=data.frame(Xt$t,Xt$y,reg5.pred,errorrate5)
predanre5
describe(errorrate1,type = 2)
describe(errorrate5,type = 2)

#output 
sumreport=data.frame(Xt$t,Xt$y,y1pred=y1pred[,1],errorrate1,reg5pred=predanre5$fit,errorrate5)
sumreport
describe(errorrate1,type = 2)
describe(errorrate5,type = 2)

plot(Xt$t,errorrate1,pch=19,cex=1.5,xlim=c(2014,2017),ylim=c(0,9),col='red',main ='Figure 4: The scatterplot of error rate for regression model between 2014 to 2017')
points(Xt$t,errorrate5,pch=19,cex=1.5,col='blue')
legend("top", legend = labs, cex = 0.8, lty = 1, lwd = 2, pch =19, col = c("red","blue"), inset = 0.01, horiz = TRUE)


plot(Xt$t,Xt$y,xlab='time',ylab='price',pch=19,cex=1.5,col='red',main ='Figure 4: The scatterplot of Shanghai Real Estate histrionic price')
lines(Xt$t,y1pred[,1],col='blue',lwd=2)
lines(Xt$t,predanre5$fit,col='orange',lwd=2)
legend("top", inset=.05,,c("Non-linear regression","Regression 5"), fill=c('blue','red'), horiz=TRUE)

