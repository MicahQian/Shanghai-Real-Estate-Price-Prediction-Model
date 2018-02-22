X=read.csv("F:\\data.csv") 
attach(X) 
plot(t,y)

y1=log(y)
a1=log(t)
reg=lm(y1~t)
summary(reg)
a1= -3.281e+02
a=exp(a1)
b= 1.681e-01 
yy=a*exp(b*t)
plot(t,y)
lines(t,yy)
plot(X)

