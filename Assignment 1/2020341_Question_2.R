rm(list=ls())

set.seed(12345)

lambda <- 10
lambda1 <- 20
t <- as.integer(readline(prompt = "Enter the time(t) of observation : "))

arr = c(1:t)
for(i in 1:t){
  arr[i] = dpois(i,lambda=lambda)
}
plot(arr,type = 'l',main = "Density of number of arrivals for lambda = 10",xlab = "Number of Arrivals",ylab = "Density");

arr1 = c(1:t)
for(i in 1:t){
  arr1[i] = dpois(i,lambda=lambda1)}
plot(arr1,type = 'l',main = "Density of number of arrivals for lambda = 20",xlab = "Number of Arrivals",ylab = "Density")

lambda2 <- 5
arr2 = c(1:t)
for(i in 1:t){
  arr2[i] = dpois(i,lambda=lambda2)}
plot(arr2,type = 'l',main = "Density of number of arrivals for lambda = 5",xlab = "Number of Arrivals",ylab = "Density")

lambda3 <- lambda + lambda2
arr3 = c(1:t)
for(i in 1:t){
  arr3[i] = dpois(i,lambda=lambda3)}
plot(arr3,type = 'l',main = "Density of number of arrivals for lambda = 10 + 5 (Joined Process)",xlab = "Number of Arrivals",ylab = "Density")


j <- 1
sum <- 0
for(i in arr3){
  sum = sum + i
  visitors[j] = sum
  j = j+1
}
plot(visitors,main = "Total Number of Visitors on both website",xlab = "Time(t)",ylab = "Number of visitors")

expdist15 <- c(1:100)
for(i in 1:t){
  val <- lambda3*i
  val = val/exp(val)
  expdist15[i] = val;
}
plot(expdist15,type='l',main = "First Interarrival for Lambda 15",xlab = "Time(t)")
plot(ecdf(expdist15),main = "CDF for First inter-arrival time",xlab = "Time(t)",ylab = "P(X <= x)")
