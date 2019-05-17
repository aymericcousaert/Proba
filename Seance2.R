source('FileAttente.R')


D<-60*12
liste <- FileMM2(20/60,15/60,D)
print(liste)
nbClient <- NbClientSysteme(liste,D)
print(nbClient)
plot(1:D,nbClient[1:D],xlab='Temps en minutes', ylab='Nombre de clients', main='Nombre de clients dans la file en fonction du temps',type='s')
