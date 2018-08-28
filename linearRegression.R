# Least sum of square is shown by the mean

x<-c(0.725,0.429,-0.372 ,0.863)

mu=mean(x)
a=c()

for (i in x) {
  
  MSS=(mu-i)^2
  # a<-MSS
  #print(MSS)
  
  a <- c(a,MSS)
}

print(a)

#Weighted means , Least Sum of sqaure

w = c(2, 2, 1, 1)
print(length(w))
xw=c()
for (i in 1:length(w)){
  print(i)
  xw<- c(xw,(w[i]*x[i]))
  }

mu1=sum(xw)/sum(w)

print(mu1)
print (xw)

print(sum(x*w))

# working with the data from galton

install.packages("UsingR")
library(UsingR); data(galton);
library(reshape2)
long <- melt(galton) 
g <- ggplot(long, aes(x = value, fill = variable)) 
g <- g + geom_histogram(colour = "black", binwidth=1) 
g <- g + facet_grid(. ~ variable) 
g
library(manipulate)

#Take the Galton dataset and find the mean, standard deviation and correlation between the parental and child heights
muChild <- mean(galton$child)
muParent <- mean(galton$parent)

stdChild <- sd(galton$child)
stdParent <- sd(galton$parent)

corrChild <- cor(galton$child,galton$parent)

#help (lm)

#Centertheparentandchildvariablesandverifythatthecenteredvariablemeansare0
x=galton$child
y=galton$parent

cx=x-mean(x)
print(mean(cx))

cy=y-mean(y)
print(mean(cy))

#
scaleX=scale(x,TRUE,TRUE)
sd(scaleX)

scaleY=y/sd(y)
sd(scaleY)


#Normalize the data for child and parent and verify if the mean=0 and std dev=1

zChild=(x-mean(x))/sd(x)
mean(zChild)
sd(zChild)

zParent=(y-mean(y))/sd(y)
mean(zParent)
sd(zParent)

#Corr(x,y)=cov(xy)/sd(x)*sd(y)

corrChildParent=cor(x,y)

#Extended form 
covChPa=sum(cx*cy)/(length(galton$child)-1)
corrChPa=covChPa/(stdChild*stdParent)



