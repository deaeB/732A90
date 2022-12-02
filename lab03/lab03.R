library(dplyr)
library(ggplot2)
library(gridExtra)
library(poweRlaw)
#Question 1
#Task 1
x=seq(0,10,0.05)

#one–sided strictly stable distribution of order 1/2 where c=2
f=function(x){
  c=2
  sapply(x,function(y){
    if(y>0){
      return(c*sqrt(2*pi)^(-1)*exp(-(c^2)/(2*y))*(y^(-3/2)))
    }else{
      return(0)
    }
  })
}

y_f=f(x)

#power–law distribution where a=1.5 and t_min=4/3
fp=function(x){
  a=1.5;t_min=4/3
  sapply(x,function(y){
    if(y>t_min){
      return(((a-1)/t_min)*(y/t_min)^(-a))
    }else{
      return(0)
    }
  })
}
y_fp=fp(x)

df=rbind(data.frame("x"=x,"y"=y_f,"class"="f"),data.frame("x"=x,"y"=y_fp,"class"="fp"))



p1=ggplot(data=df,aes(x=x,y=y))+geom_point(aes(color=class))+
  labs(tag="Fig. 1")+
  scale_color_discrete(
    name="distribution",
    labels=c("one–sided strictly stable distribution","power–law distribution")
  )
p1

"
The power–law distribution can not be used just by itself,because the two distribution don't have the same support.
In (0,t_min),the density of the power–law distribution is 0,but for one–sided strictly stable distribution of order 1/2 it isn't.The power–law distribution can't generate the proper samples in accept-reject method in this interval.
We can apply a uniform distribution on the interval (0,t_min) as a majorizing function


"


#Task 2

sampling=function(samp_num,c,t_min,alpha){
  label=TRUE
  reject=0
  while(label){
    Y=c(rplcon(floor(samp_num*6/7),t_min,alpha),runif(floor(samp_num/7),0,t_min))
    X=sapply(Y,function(Y){
      X=c()
      U=runif(1,0,1)
      temp=ifelse(fp(Y)==0,1,f(Y)/fp(Y))
      if(U<=temp){
        X=c(X,Y)
      }
    })
    label=FALSE
  }
  return(X)
}

samp=data.frame("sample"=as.numeric(sapply(unlist(sampling(500,2,4/3,1.5)),function(x){return(x)})))

p2=ggplot(samp,aes(x=sample))+geom_density()+xlim(c(0,10))+labs(title="The sampler density",tag="Fig. 2")

df_f=df%>%filter(class=="f")

p3=ggplot(data=df_f,aes(x=x,y=y))+geom_line()+labs(title="The target density",tag="Fig. 3")

plot_comb=grid.arrange(p2,p3)

plot_comb


#Task 3

#c=1
samp_c1=data.frame("sample"=as.numeric(sapply(unlist(sampling(500,2,4/3,1.5)),function(x){return(x)})))

#c=1.5

#c=2

#c=3

#c=4



#Question 2
#Task 1
r_laplace=function(gen_num,mu,alpha){
  unif=runif(gen_num,0,1)
  temp=sapply(unif,function(unif){
    if(unif>=0.5){
      return((-1/alpha)*log(2*(1-unif))+mu)
    }else{
      return((log(2*unif)/alpha)+mu)
    }
  })
  return(temp)
}

p4=ggplot(data=data.frame("y"=r_laplace(10000,0,1)),aes(x=y))+geom_histogram(bins=50)+labs(title="Laplace distribution generate by Inverse CDP method")
p4

"
compare to the plot of laplace distribution probability density function(it can be thought of as two exponential distributions spliced together along the abscissa),the result seems resonable.
"




"
#Task 2
ar_norm<-function(c){
  x<-NA
  num.reject<-0
  while (is.na(x)){
    y<-r_laplace(1,0,1) 
    u<-runif(1)
    if (u<=dnorm(y,0,1)/(c*(exp(-1*abs(y))/2))){x<-y}
    else{num.reject<-num.reject+1}
  }
  c(x,num.reject)
}

#Plot
df_norm=data.frame(t(data.frame(sapply(rep(sqrt((2*exp(1))/pi),2000),ar_norm))))
colnames(df_norm)=c("sample","reject")
p4=ggplot(data=df_norm,aes(x=sample))+geom_histogram(bins=50)+labs(title="Normal distribution generate by Acceptance/rejection method")
p4

#Rejection rate
mean_r=sum(df_norm[,2])/(2000+sum(df_norm[,2]))
ER=1-(1/sqrt((2*exp(1))/pi))

cat("The average rejection rate: ",mean_r,"\nThe expected rejection rate: ",ER,"\nThe difference:",abs(ER-mean_r))

#Plot
p5=ggplot(data=data.frame("x"=rnorm(2000,0,1)),aes(x=x))+geom_histogram(bins=50)+labs(title="Normal distribution generate by rnorm")
plot_comb1=grid.arrange(p4,p5)


"











