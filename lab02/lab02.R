library(dplyr)
library(ggplot2)


# Assignment 1
## Problem 1
interpolates<-function(myfun,param_x){
  stopifnot(param_x<=1 & param_x>=0)
  
  a=c(1,1,1)
  loss<-function(a,myfun,param_x){
    the_loss=(myfun(param_x[1])-(a[1]+a[2]*param_x[1]+a[3]*(param_x[1]^2)))^2+
      (myfun(param_x[2])-(a[1]+a[2]*param_x[2]+a[3]*(param_x[2]^2)))^2+
      (myfun(param_x[3])-(a[1]+a[2]*param_x[3]+a[3]*(param_x[3]^2)))^2
    return(the_loss)
  }
  optim_result=optim(par=a,fn=loss,myfun=myfun,param_x=param_x,method=c("BFGS"))
  return(optim_result$par)
}

## Problem 2
approximate<-function(cut_num,myfun){
  intervals=seq(0,1,length.out=cut_num+1) #divided intervals into 3 parts
  result=data.frame()
  for(i in 1:cut_num){
    interv=c(intervals[i],(intervals[i]+intervals[i+1])/2,intervals[i+1])
    par=interpolates(myfun,interv)
    result=rbind(result,c(interv,par))
  }
  colnames(result)=c("x0","x1","x2","a0","a1","a2")
  return(result)
}

## Problem 3
fun1<-function(x){
  return(-x*(1-x))
}

fun2<-function(x){
  return(-x*sin(10*pi*x))
}

result1=approximate(100,fun1)%>%mutate(value=(a0+a1*x1+a2*x2^2))
result2=approximate(100,fun2)%>%mutate(value=(a0+a1*x1+a2*x2^2))

###plot1
p1<-ggplot(data=result1,aes(x=x1,y=value))+
  geom_line(color="blue")+
  geom_function(fun=fun1,colour="red")+
  labs(x="x",y='y',
       title="The Plot of f1",
       subtitle = "red line-the original function,blue line-approximation function",
       tag = "Fig. 1")+
  theme_set(theme_bw())
p1

###plot2
p2<-ggplot(data=result2,aes(x=x1,y=value))+
  geom_line(color="blue")+
  geom_function(fun=fun2,colour="red")+
  labs(x="x",y='y',
       title="The Plot of f2",
       subtitle = "red line-the original function,blue line-approximation function",
       tag = "Fig. 2")+
  theme_set(theme_bw())
p2

#Q:……………………………………
"
piecewise–parabolic interpolaters seems fits the original functions pretty well.The blue line and the red line basically coincide, but there are still some errors.
"


# Assignment 2
## Problem 1
load("data.RData")

## Problem 2
mu=mean(data)
sigma=sqrt(sum((data-mu)^2)/length(data))
mu
sigma

## Problem 3
minusLogLikelihood<-function(params){
  mu=params[1]
  sigma=params[2]
  n=length(data)
  result=-(-(n/2)*log(2*pi*sigma^2)-(1/(2*sigma^2))*sum((data-mu)^2))
  return(result)
}

#求梯度（对mu和对sigma）
grad<-function(params){
  mu=params[1]
  sigma=params[2]
  n=length(data)
  result=c(-sum(data-mu)/(sigma^2),(n/sigma)-sum((data-mu)^2)/sigma^3)
  return(result)
}

#Conjugate Gradient method with gradient
cg_grad=optim(par=c(0,1),fn=minusLogLikelihood,gr=grad,method="CG")
cg_grad

#BFGS with gradient
BFGS_grad=optim(par=c(0,1),fn=minusLogLikelihood,gr=grad,method="BFGS")
BFGS_grad

#Conjugate Gradient method without gradient
cg_nongrad=optim(par=c(0,1),fn=minusLogLikelihood,method="CG")
cg_nongrad

#BFGS without gradient
BFGS_nongrad=optim(par=c(0,1),fn=minusLogLikelihood,method="BFGS")
BFGS_nongrad


#Q:Why it is a bad idea to maximize likelihood rather than maximizing log–likelihood?
"
Because the probability values are all between [0,1], the multiplication of the probability will become a small value, which may cause underflow, especially when the data set is large, the joint probability will be tends to 0, which is very unfavorable for subsequent calculations.
"

## Problem 4
  


















