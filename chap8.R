data <- c(4,3,4,5,5,2,3,1,4,0,1,5,5,6,5,4,4,5,3,4)
data
q_area <- seq(0.1,0.99,0.01)
q_area
sample(q_area,1)
coin <- c(0.01,-0.01)
sample(coin,1)
logL <- function(m) sum(dbinom(data,size=8,prob=m, log=TRUE))
logL(0.30)

MHM <- function(iter){
  param_vec <- c()
  init_q <- sample(q_area,1)
  param_vec <- append(param_vec,init_q)
  for (n in 1:iter){
    q_new <- param_vec[n] + sample(coin,1)
    if (logL(param_vec[n]) < logL(q_new)){
    param_vec <- append(param_vec,q_new)
    }
    else{
      r <- exp(logL(q_new)-logL(param_vec[n]))
      HorT <- sample(c(1,0),size=1,prob=c(r,1-r))
      if (HorT == T){param_vec <- append(param_vec, q_new)}
      else{param_vec <- append(param_vec, param_vec[n])}
    }
  }
  return(param_vec)
}
chain_trace_1 <- MHM(3000)
plot(chain_trace_1,type='l')
hist(chain_trace_1[200:3000])

trace_2 <- MHM(3000)
trace_3 <- MHM(3000)
plot(chain_trace_1,type='l',ylim=c(0.2,0.7),xlab='iter',ylab='parameter' )
par(new=TRUE)
plot(trace_2,type='l',col=2,ylim=c(0.2,0.7),xlab='',ylab='')
par(new=TRUE)
plot(trace_3,type='l',col=3,ylim=c(0.2,0.7),xlab='',ylab='')
