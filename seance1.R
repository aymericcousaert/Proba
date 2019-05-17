# au prealable, vous devez executer l'instruction suivante
# install.packages('randtoolbox')

library(randtoolbox)
source('generateurs.R')

sVN <- 9721
sMT <- 2504
Nsimu <- 1000
Nrepet <- 20


vn <- VonNeumann(Nsimu,Nrepet,sVN)
mt <- MersenneTwister(Nsimu,Nrepet,sMT)
rd <- RANDU(3,1000)
sm <- StandardMinimal(3,1000)

par(mfrow=c(1,4))
hist(mt[,1],xlab='',main='Mersenne Twister')
hist(vn[,1],xlab='',main='Von Neumann')
hist(rd[,1],xlab='',main='RANDU')
hist(sm[,1],xlab='',main='Standard Minimal')

par(mfrow=c(1,2))
plot(mt[1:(Nsimu-1),1],mt[2:Nsimu,1],xlab='MT(i)', ylab='MT(i+1)', main='Mersenne Twister')
plot(vn[1:(Nsimu-1),1],vn[2:Nsimu,1],xlab='VN(i)', ylab='VN(i+1)', main='Von Neumann')
par(mfrow=c(1,2))
plot(rd[1:(Nsimu-1),1],rd[2:Nsimu,1],xlab='RD(i)', ylab='RD(i+1)', main='RANDU')
plot(sm[1:(Nsimu-1),1],sm[2:Nsimu,1],xlab='SM(i)', ylab='SM(i+1)', main='Standard Minimal')

test <- rep(619,1)



