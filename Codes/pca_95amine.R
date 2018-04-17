##
rm(list=ls())
# setwd('c:/Study/My projects/Statchem-Diudea/Codes')
source('RobustQSAR_functions.R')

library(ddalpha)

# combined descriptors
combined95 = read.csv("../Data/Combined-descriptors-95.csv")
y95 = as.numeric(combined95[,2])
X95 = as.matrix(combined95[,-(1:2)])

# apply robust scaling
delta = 1e-3
spa = spatial.median(X95, delta)
mu = spa$mu
ep = spa$ep
sigma.vec = apply(X95, 2, mad)
X95 = as.matrix(scale(X95, mu, sigma.vec))
# X95 = as.matrix(scale(X95, mu, scale=F))
which.na = which(is.na(apply(X95,2,var)))
# which.na = which(apply(X95,2,var) < 1e-3)
X95 = X95[,-which.na]
names95 = names(combined95)[-(1:2)][-which.na]
df95 = data.frame(cbind(y95, X95))

n = nrow(X95)
p = ncol(X95)

## Principal Component Analysis
set.seed(04172018)
Xd = X95
# depth = depth.projection(X95, X95)
# depth = max(depth) - depth
# for(i in 1:n)
# {
#   z = sqrt(sum((Xd[i,  ])^2))
#   if(z > ep)
#   {
#     Xd[i,  ] = depth[i] * (Xd[i,  ]  )/z
#   }
# }
svd95 = svd(Xd)
names95[order(abs(svd95$v[,1]))][1:10]
svd95$v[order(abs(svd95$v[,1])),1][1:10]

names95[order(abs(svd95$v[,2]))][1:10]
svd95$v[order(abs(svd95$v[,2])),2][1:10]

names95[order(abs(svd95$v[,3]))][1:10]
svd95$v[order(abs(svd95$v[,3])),3][1:10]

names95[order(abs(svd95$v[,4]))][1:10]
svd95$v[order(abs(svd95$v[,4])),4][1:10]
