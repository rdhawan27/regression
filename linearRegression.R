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

