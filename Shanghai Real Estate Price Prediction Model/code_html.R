library(R2HTML)
target <- HTMLInitFile(file.path("F:\\HTML"),filename="REPORT", BackGroundColor="#BBBBEE") 
---
title: "Shanghai Real Estate Price Prediction Model"
author: "Jiadong Qian"
date: "10/4/2018"
output:
  html_document
  self_contained: yes
  toc: yes
  toc_float: yes
---
    
<style type="text/css">
body{ /* Normal  */
    font-size: 12px;
    font-family: Times New Roman;
    color: Black;
  }
h1.title {
  font-size: 38px;
  font-family: Times New Roman;
  color: Black;
}
h1 { /* Header 1 */
    font-size: 20px;
  font-family: Times New Roman;
  color: DarkBlue;
}
h2 { /* Header 2 */
    font-size: 18px;
  font-family: Times New Roman;
  color: DarkBlue;
}
h3 { /* Header 3 */
    font-size: 16px;
  font-family: "Times New Roman", Times, serif;
  color: DarkBlue;
}
code.r{ /* Code block */
    font-size: 12px;
}
pre { /* Code block - determines code spacing between lines */
    font-size: 14px;
  </style>

```{r}
#read the train data
X=read.csv("F:\\data.csv") 
#read the test data
Xt=read.csv("F:\\data2.csv") 
```

```{r}
#draw scatterplot of data
attach(X) 
plot(t,y)
```
```{r}
#transform a non-linear model (exponential model) to a linear regression model 
y1=log(y)
a1=log(t)
```

```{r}
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
```

```{r}
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
```

```{r}
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
```

```{r}
#run stepwise regression for each variable
reg5=step(lm(y~x1+x2+x3+x4+x5+x6+x7),direction="both")
summary(reg5)

#run backward regression for each variable
reg6=step(lm(y~x1+x2+x3+x4+x5+x6+x7),direction="backward")
summary(reg6)
```

```{r}
#make prediction for Regression 1 in next three years
pred=data.frame(t=Xt$t)
yy.pred=predict(yy,pred,interval="prediction",level=0.95)
yy.pred
```


```{r}
#read the test data
Xt=read.csv("F:\\data2.csv") 
```

```{r}
#make prediction for Regression 1 in next three years
pred1=data.frame(t=Xt$t)
reg.pred=predict(reg,pred1,interval="prediction",level=0.95)
y1pred=exp(reg.pred)
y1pred

#make predictions for Regression 5 in next three years
pred5=data.frame(x1=Xt$x1,x2=Xt$x2,x3=Xt$x3,x6=Xt$x6)
reg5.pred=predict(reg5,pred5,interval="prediction",level=0.95)
reg5.pred

#make predictions for Regression 6 in next three years
pred6=data.frame(x1=Xt$x1,x2=Xt$x2,x3=Xt$x3,x6=Xt$x6)
reg6.pred=predict(reg6,pred6,interval="prediction",level=0.95)
reg6.pred


#predition analysis for Regression 1
errorrate1= abs((y1pred[,1]-Xt$y)/Xt$y)*100
predanre1=data.frame(Xt$t,Xt$y,y1pred,errorrate1)
predanre1

#predition analysis for Regression 5
errorrate5= abs((reg5.pred[,1]-Xt$y)/Xt$y)*100
predanre5=data.frame(Xt$t,Xt$y,reg5.pred,errorrate5)
predanre5
```