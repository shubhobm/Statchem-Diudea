##
rm(list=ls())
# setwd('c:/Study/My projects/Statchem-Diudea/Codes')
source('RobustQSAR_functions.R')

library(ddalpha)

# combined descriptors
combined508 = read.csv("../Data/Combined-descriptors-508.csv")
y508 = as.numeric(combined508[,2])
X508 = as.matrix(combined508[,-(1:2)])

# apply robust scaling
delta = 1e-3
spa = spatial.median(X508, delta)
mu = spa$mu
ep = spa$ep
sigma.vec = apply(X508, 2, mad)
X508 = as.matrix(scale(X508, mu, sigma.vec))
# X508 = as.matrix(scale(X508, mu, scale=F))
which.na = which(is.na(apply(X508,2,var)))
# which.na = which(apply(X508,2,var) < 1e-3)
X508 = X508[,-which.na]
names508 = names(combined508)[-(1:2)][-which.na]
df508 = data.frame(cbind(y508, X508))

n = nrow(X508)
p = ncol(X508)

## Principal Component Analysis
Xd = X508
# depth = depth.projection(X508, X508, seed=04172018)
# depth = max(depth) - depth
# for(i in 1:n)
# {
#   z = sqrt(sum((Xd[i,  ])^2))
#   if(z > ep)
#   {
#     Xd[i,  ] = depth[i] * (Xd[i,  ]  )/z
#   }
# }
svd508 = svd(Xd)
names508[order(abs(svd508$v[,1]))][1:10]
svd508$v[order(abs(svd508$v[,1])),1][1:10]

names508[order(abs(svd508$v[,2]))][1:10]
svd508$v[order(abs(svd508$v[,2])),2][1:10]

names508[order(abs(svd508$v[,3]))][1:10]
svd508$v[order(abs(svd508$v[,3])),3][1:10]

names508[order(abs(svd508$v[,4]))][1:10]
svd508$v[order(abs(svd508$v[,4])),4][1:10]
