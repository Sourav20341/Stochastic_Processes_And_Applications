rm(list=ls()) 

a <- rbinom(100,1,0.8)
m <- 5
M <- 10
steps = NULL
steps[1] <- 5
i <- 1
j<-2
while (TRUE){
  if(a[i] == 1){
    if(steps[j-1]+1 <= 10){
      steps[j] = steps[j-1]+1
    }
    else{
      break
    }
  }
  else{
    if(steps[j-1]-1 >= 0){
      steps[j] = steps[j-1]-1
    }
    else{
      break
    }
  }
  j <- j+1
  i <- i+1
}
plot(steps, type='l',lty = 1,ylim = c(1,10),ylab = "Amount (in Dollars)",xlab = "Time")

steps = NULL
steps[1] <- 5
i <- 1
j<-2
while (TRUE){
  if(a[i] == 0){
    if(steps[j-1]+1 <= 10){
      steps[j] = steps[j-1]+1
    }
    else{
      break
    }
  }
  else{
    if(steps[j-1]-1 >= 0){
      steps[j] = steps[j-1]-1
    }
    else{
      break
    }
  }
  j <- j+1
  i <- i+1
}
plot(steps, type='l',lty = 1,ylim = c(1,10),ylab = "Amount (in Dollars)",xlab = "Time")

