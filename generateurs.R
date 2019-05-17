  VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
      numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}


MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}


binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

RANDU <- function(seed,k)
{
  result <- rep(0,k)
  result[1]<-seed
  
  a <- 65539
  b <- 0
  m <- 2^31
  
  for(i in 2:(k+1))
  {
    result[i]<-(a*result[i-1]+b)%%m
  }
  result <- result[2:(k+1)]
  result <- matrix(result,Nsimu,1)
  return(result)
}

StandardMinimal <- function(seed,k)
{
  result <- rep(0,k)
  result[1]<-seed
  
  a <- 16807
  b <- 0
  m <- 2^31 - 1 
  
  for(i in 2:(k+1))
  {
    result[i]<-(a*result[i-1]+b)%%m
  }
  result <- result[2:(k+1)]
  result <- matrix(result,Nsimu,1)
  return(result)
}

Frequency <- function(x, nb)
{
  somme <- 0
  for (i in (1:length(x)))
  {
    tab <- binary(x[i])
    tabUpdated <- tab
    for(k in length(tab):(length(tab)-nb+1))
    {
      tabUpdated[k] <- 2*tab[k] - 1
      somme <- somme + tabUpdated[k]
    }
  }
  Sobs <- abs(somme) / sqrt(length(x)*nb)
  Pval <- 2*(1-pnorm(Sobs))
  return(Pval)
}

Runs <- function(x,nb)
{
  somme <- 0
  tab <- rep(0,0)
  for (i in (1:length(x)))
  {
    nbBinaire <- binary(x[i])
    tabPartiel <- rep(0,0)
    for(k in (length(nbBinaire)-nb+1) : length(nbBinaire) ){
       tabPartiel[k-length(nbBinaire)+nb] <- nbBinaire[k]    
    }
    tab = c(tab,tabPartiel)
  }
    for(k in 1:length(tab))
    {
      if (tab[k] == 1)
      {
        somme <- somme + 1
      }
    }
  
  n <- (length(x)*nb)
  pi <- somme/n
  if (abs((pi - 0.5)) > 2/sqrt(n))
  {
    return (0)
  }
  else
  {
    r <- 1
    for(k in 1: (length(tab)-1) )
    {
      if (tab[k] != tab[k+1])
      {
        r <- r + 1
      }
    }
  }
  Pval <- 2*(1-pnorm(   abs(r - 2*n*pi*(1-pi))/ (2*sqrt(n)*pi*(1-pi))   ))
  return (Pval)
}




