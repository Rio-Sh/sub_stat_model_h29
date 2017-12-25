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
  init_q <- sample(q_area,1)# q_areaからランダムに一つ
  param_vec <- append(param_vec,init_q)
  for (n in 1:iter){
    q_new <- param_vec[n] + sample(coin,1)#n回目のqに確率で+0.01か-0.01
    if (logL(param_vec[n]) < logL(q_new)){
    param_vec <- append(param_vec,q_new)#logL(q)<logL(q_new)ならq_newへ移動
    }
    else{#logL(q)>logL(q_new)のとき
      r <- exp(logL(q_new)-logL(param_vec[n]))#尤度比 r = L(q_new)/L(q)
      HorT <- sample(c(TRUE,FALSE),size=1,prob=c(r,1-r))#確率rでTRUEになる
      if (HorT == TRUE){param_vec <- append(param_vec, q_new)}#TRUEがでたらq_newへ移動
      else{param_vec <- append(param_vec, param_vec[n])}#FALSEならqは移動しない
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

