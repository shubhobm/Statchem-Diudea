##
rm(list=ls())
# setwd('D:/Study/My projects/Statchem-Diudea/Codes')
source('RobustQSAR_functions.R')

## load outputs
load('err95.Rda')
load('../Data/lta98.rda')
rf.preds = err.mat[1:100,5]
rf.q2 = 1 - 5*rf.preds/sum(lta98$Y^2)
plot(rf.q2, ylab="q2", xlab="split", type='h', col='black', lwd=2, ylim=c(0,1),
     main="Random forest prediction for 95 amines data")

m = mean(rf.q2)
s = sd(rf.q2)
colors = rep(grey(0.8), 100)
for(i in 1:100){
  if(rf.q2[i]<m-s){
    colors[i] = "red"
  }
  if(rf.q2[i]>m+s){
    colors[i] = "darkgreen"
  }
}
plot(rf.q2, ylab="q2", xlab="split", type='h', col=colors, lwd=2, ylim=c(0,1),
     main="Random forest prediction for 95 amines data")
abline(h = m, lwd=2)
abline(h = m-s, lwd=2, lty=2)
abline(h = m+s, lwd=2, lty=2)
