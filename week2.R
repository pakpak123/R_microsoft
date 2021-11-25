rm(list-ls())
ls()

x=c(1,2,3,4)
w=c(0.1,0.2,0.3,0.4)
print(w*x)
net = sum(w*x)
print(net)

setwd=("C:\Users\Asus\OneDrive - KMITL\Documents\R\win-library\4.0\Metrial02\Metrial02")
#LOAD DATA
ga = read.csv("group_a.txt", header=T, sep="\t")
gb = read.csv("group_b.txt", header=T, sep="\t")
#PLOT DATA
plot.new()
plot(ga[,1:2],col="red")
lines(gb[,1:2],type="p",col="blue")
#CHECK RANGE OF DATA
summary(ga)
summary(gb)

###############################################
perceptron = function(x)
{	
  err = 1
  iter = 0
  maxi = length(x$x1)
  while(err>0.01 && iter<5000)
  {
    w1 = runif(1)	#RANDOM VALUE		
    w2 = runif(1)	#RANDOM VALUE
    
    err = 0
    for(i in 1:maxi)
    {
      #print(x[i,1])
      x1 = x[i,1]
      x2 = x[i,2]
      sum = x1*w1 + x2*w2
      #print(sum)	
      
      yh = step_func(sum,100)
      
      yt = x[i,3]
      if(yh==yt)
      {
        #print("pass")	
      }else
      {
        #print("not pass")
        err = err+1
      }
    }
    err = err/maxi
    #print(err)
    iter = iter+1
    #print(iter)
  }
  #print(iter)
  #print(c(w1,w2))
  listReturn = list("weight" = c(w1,w2), "error" = err)
  return(listReturn)
} 

########################
logistic = function (x) 
{
  1/(1 + exp(-x))
}
x = seq(-5,5,0.1)
maxx = length(x)
y = 1:maxx
x = logistic(x)
plot(y, x)

#########
library("neuralnet")
AND = c(0,0,0,1)
truthtable = expand.grid(c(0,1), c(0,1))
AND.data <- data.frame(truthtable, AND)
print(AND.data)
net <- neuralnet( AND~Var1+Var2, AND.data, hidden=0, rep=1)
print(net)
plot(net)

#####################
var1 = c(0,1,0,1)
var2 = c(0,0,1,1)
datatest = data.frame(var1,var2)
pred <- predict(model = net, datatest)
pred

##################
library("neuralnet")
OR = c(0,0,0,1)
truthtable = expand.grid(c(0,1), c(0,1))
OR.data <- data.frame(truthtable,OR)
print(OR.data)
net <- neuralnet( OR~Var1+Var2, OR.data, hidden=0, rep=1)
print(net)
plot(net)

logistic = function (x) 
{
  1/(1 + exp(-x))
}
x = seq(-5,5,0.1)
maxx = length(x)
y = 1:maxx
x = logistic(x)
plot(y, x)
