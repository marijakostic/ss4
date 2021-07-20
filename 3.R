# TRECI ZADATAK

# Implementiramo Bernulijevu raspodelu sa parametrom p

rbernuli <- function(p)
{
  # Generisemo prvo slucajan broj iz (0,1)
  U <- runif(1)
  # Zatim vracamo slucajan broj iz Ber(p)
  ifelse(U < p, 1, 0)
}

# Uzorak iz raspodele X kada je Y poznato

uzorakX_poznatoY <- function(y)
{
  if(y==0)
  {
    x <- rbernuli(0.375) # vraca 1 sa verovatnocom 0.375,inace vraca 0
  }
  else
  {
    x <- rbernuli(0.5)
  }
  return(x)
}

# Uzorak iz raspodele Y kada je X poznato

uzorakY_poznatoX <- function(x)
{
  if(x==0)
  {
    y <- rbernuli(1/6) # vraca 1 sa verovatnocom 0.375,inace vraca 0
  }
  else
  {
    y <- rbernuli(0.25)
  }
  return(y)
}


set.seed(100)
niter <- 10000
X <- rep(0,niter)
Y <- rep(0,niter)
X[1]=1
Y[1]=1 # pocinjemo od (1,1)

for(i in 2:niter)
{
  X[i] <- uzorakX_poznatoY(Y[i-1])
  Y[i] <- uzorakY_poznatoX(X[i])
}

# Nas uzorak
Uzorak <- data.frame(X=X,Y=Y)

# Ispisacemo sta se dobija za prvih 10 iteracija
head(Uzorak,10)

# Jos cemo proveriti koliko se slaze sa pocetnom pastodelom
table(data.frame(X=X,Y=Y))/niter
