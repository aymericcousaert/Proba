FileMM1 <- function(lambda, mu, D){
  
  arrivees <- rep(0,0)
  tAct <- rexp(1,rate = lambda)
  tDerniereArrivee <- 0
  i <- 1
  while((tAct + tDerniereArrivee) < D){
    arrivees[i] <- tAct + tDerniereArrivee
    tDerniereArrivee <- arrivees[i]
    tAct <- rexp(1,rate = lambda)
    i<-i+1
  }
  
  departs <- rep(0,0)
  departs[1]<-arrivees[1] + rexp(1,rate = mu)
  for(k in 2:length(arrivees)){
    tService <- rexp(1,rate = mu)
    if(departs[k-1] < arrivees[k] ){
      if((arrivees[k] + tService )<D){
        departs[k]<-(arrivees[k] + tService)
      } else {
        break
      }
    } else {
      if((departs[k-1] + tService)<D){
        departs[k] <- (departs[k-1] + tService)
      } else {
        break
      }
    }
  }
  
  ma_liste <- list(arrivees,departs)
  return (ma_liste)
}

NbClientSysteme <- function(list,D){
  tabClasse <- rep(0,D)
  for (i in 1:length(list[[1]])  ) {
   tabClasse[floor(list[[1]][i])+1] <- tabClasse[floor(list[[1]][i])+1] + 1
  }
  for (i in 1:length(list[[2]])  ) {
    tabClasse[floor(list[[2]][i])+1] <- tabClasse[floor(list[[2]][i])+1] - 1
  }
  for(k in 2:length(tabClasse)){
    tabClasse[k]<-tabClasse[k] + tabClasse[k-1]
  }
  return(tabClasse)
}

Little <- function(list,D,nbClient){
  EN <- 0
  for (i in (1:length(nbClient))) {
    EN <- EN + nbClient[i]
  }
  EN <- EN / length(nbClient)
  EW <- 0
  for (i in (1:min(   length(list[[1]])   ,   length(list[[2]])   ))) {
    EW <- EW + list[[2]][i] - list[[1]][i]
  }
  EW <- EW / min(   length(list[[1]])   ,   length(list[[2]])   )
  return(list(EN,EW))
}



