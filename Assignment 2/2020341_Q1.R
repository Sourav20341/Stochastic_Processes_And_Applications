rm(list=ls()) 
set.seed(1234)
# discrete markov chain simulator 
simulator <- function(number_of_steps,P,num_of_states,starting_state){
  M <- matrix(0,number_of_steps,1)
  steps_arr <- as.vector(M)
  steps_arr[1] <- starting_state
  for(i in 2:number_of_steps){
    state <- P[steps_arr[i-1],]
    d <- rmultinom(num_of_states,1,state)
    count = 1
    for(j in  d){
      if(j == 1){
        break
      }
      count <- count + 1
    }
    steps_arr[i] <- count
  }
  return (steps_arr)
}

# (a)

times = 5
steps = 50
Distribution = matrix(0,nrow = steps,ncol = times)

P1 <- c(0.3, 0.7)
P2 <- c(0.5, 0.5)
P <- matrix(c(P1,P2),ncol=2,nrow=2)

for (i in 1:times){
  Distribution[,i] = simulator(steps,P,2,1)
}
plot(c(1:50),lty = c(0:10),Distribution[,1],ylim = c(0,2), type = "l", col = "red",xlab = "Time",ylab = "State")
lines(c(1:50), Distribution[,2],col = "blue")
lines(c(1:50), Distribution[,3],col = "green")
lines(c(1:50), Distribution[,4],col = "cyan")
lines(c(1:50), Distribution[,5],col = "orange")
legend(x = "bottomleft",1, 2, legend=c("1st time", "2nd time","3rd time","4th time","5th time"), fill = c("red","blue","green","cyan","orange"))

# (b)
P10 <- NULL
P20 <- NULL
P50 <- NULL
P_matrix = matrix(c(c(1,0),c(0,1)),ncol = 2,nrow = 2)
for (i in 1:50){
  P_matrix = P_matrix %*% P
  if(i == 10){
    P10 <- P_matrix
  }
  if(i == 20){
    P20 <- P_matrix
  }
  if(i == 50){
    P50 <- P_matrix
  }
}
print("Matrix P10 will be : ")
print(P10)

print("Matrix P20 will be : ")
print(P20)

print("Matrix P50 will be : ")
print(P50)

