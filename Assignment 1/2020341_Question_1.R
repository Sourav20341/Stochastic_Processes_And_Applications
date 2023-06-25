rm(list=ls())

set.seed(12345)

p <- 0.4
p1 <- 0.9
t <- as.integer(readline(prompt = "Enter the number of samples t : "))

bernoulli.data1 <- sample(0:1,t,replace=TRUE,prob=c((1-p),p))
bernoulli.data2 <- sample(0:1,t,replace=TRUE,prob=c((1-p1),p1))

sum <- 0
array <- c(1:t)
for(i in 1:t){
  sum = sum + ((1-p)**(i-1) * p)
  array[i] = sum;
}

sum <- 0
array3 <- c(1:t)
for(i in 1:t){
  sum = sum + ((1-p1)**(i-1) * p1)
  array3[i] = sum;
}

array1 <- c(1:t)
count <- 0
for(i in 1:t){
  if(bernoulli.data1[i] == 1){
    count <- count+1
  }
  array1[i] = count;
}

array2 <- c(1:t)
count1 <- 0
for(i in 1:t){
  if(bernoulli.data2[i] == 1){
    count1 <- count1+1
  }
  array2[i] = count1;
}

# 1 a
plot(bernoulli.data1,ylab = "Success(1) / Failure(0)",xlab = "Time(t)",main = "Scatter Plot for our process with arrival probability = 0.4")

# 1 b
plot(array,col = "red",type='l',xlab = "First inter-arrival time",ylab = "P(X1 <= x)",main ="Cummulative distribution of first inter-arrival time")


# 1 c


plot(bernoulli.data1,col = "red",main = "Sample Distribution from p = 0.4 and p = 0.9",ylab = "Success(1)/Failure(0)",xlab = "Time(t)")
points(bernoulli.data2,col='blue')
legend(x = "center", box.col = "green", box.lwd = 2 , title="Probabilities",legend=c("For p = 0.9", "For p = 0.4"), fill = c("blue","red"))

plot(array2,col = "blue",main = "Number of Arrivals",ylab = "Total number of arrivals",xlab = "Time(t)")
points(array1,col = "red")
legend(x = "topleft", box.col = "black", box.lwd = 2 , title="Probabilities",legend=c("For p = 0.9", "For p = 0.4"), fill = c("blue","red"))

plot(density(bernoulli.data2),col = "blue",main = "Density of Arrivals",xlab = "Time(t)")
points(density(bernoulli.data1),col = "red",type = 'l')
legend(x = "topleft", box.col = "yellow", box.lwd = 2 , title="Probabilities",legend=c("For p = 0.9", "For p = 0.4"), fill = c("blue","red"))

plot(array,col = "red",type='l',xlab = "First inter-arrival time",ylab = "P(X1 <= x)",main ="Cummulative distribution of first inter-arrival time")
points(array3,type="l",col = "blue")
legend(x = "center", box.col = "pink", box.lwd = 2 , title="Probabilities",legend=c("For p = 0.9", "For p = 0.4"), fill = c("blue","red"))

