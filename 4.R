# CETVRTI ZADATAK

# apriorna raspodela za p je U[0,1]
prior <- function(p)
{
  if((p<0)||(p>1))
  {
    return(0)
  }
  else
  {
    return(1)
  }
}

# funkcija verodostojnosti

likelihood <- function(p,nAA,nAa,naa)
{
  L <- p^(2*nAA)*(2*p*(1-p))^nAa*(1-p)^(2*naa)
  return(L)
}


MCMCsampler <- function(nAA,nAa,naa,iter,start_valute,prop_sd) 
{
  p <- rep(0,iter)
  p[1] <- start_valute # prva vrednost vektora ce biti unapred zadata vrednost start_valute,a ostale dobijamo prolazeci kroz petlju
  
  for(i in 2:iter)
  {
    trenutno_p <- p[i-1] # trenutna vrednost p
    novo_p <- trenutno_p + rnorm(1,0,prop_sd) # novu vrednost za p dobijamo dodavanjem suma iz N(0,prop_sd) raspodele
    q <-prior(novo_p)*likelihood(novo_p,nAA,nAa,naa)/(prior(trenutno_p)*likelihood(trenutno_p,nAA,nAa,naa)) # aposteriorna raspodela
    
    if (runif(1)<q)
    {
      p[i] <- novo_p
    }
    else
    {
      p[i] <-trenutno_p
    }
  }
  return(p)
}

# Sada cemo pokrenuti za nAA=50,nAa=21,naa=29, 10000 iteracija sa pocetnom vrednoscu 0.5 i sirinom raspodele predloga 0.01

X <- MCMCsampler(50,21,29,10000,0.5,0.01)
head(X,10)

# Aposteriorna raspodela za p je Beta raspodela B(122,80)

Y <- seq (0,1,length=1000) # segment [0,1] delimo na 1000 jednakih delova
hist(X, probability = T,xlim = c(0.4,0.8),col="lightblue",ylim=c(0,20))

lines(Y,dbeta(Y,122,80),col="steelblue4")

# Pokrenucemo algoritam za drugu pocetnu vrednost 0.1, i manji br iteracija 1000

X2 <- MCMCsampler(50,21,29,1000,0.1,0.01)
head(X2,10)
Y2 <- seq (0,1,length=1000) 
hist(X2, probability = T,xlim = c(0.1,0.8),col="lightblue",ylim=c(0,20))
lines(Y2,dbeta(Y2,122,80),col="steelblue4")


# crtamo grafik lanca(vremenske serije)

Y <- seq (0,1,length=1000) # segment [0,1] delimo na 1000 jednakih delova
plot(X2,type="l")

plot(X2[100:1000],type="l")
plot(X2[150:1000],type="l")

# sa grafika vidimo da se prvih 100-150 vrednosti moze odbaciti kao burn-in,



