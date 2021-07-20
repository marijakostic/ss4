# PETI ZADATAK

# Dati su podaci:
Y <- c(4,5,4,1,0,4,3,4,0,6,3,3,4,0,2,6,3,3,5,4,5,3,2,4,4,1,5,5,3,4,2,5,2,2,3,4,2,1,3,2,
       2,1,1,1,1,3,0,0,1,0,1,1,0,0,3,1,0,3,2,2,0,1,1,1,0,1,0,1,0,0,0,2,1,0,0,0,1,1,0,2,
       3,3,1,1,2,1,1,1,1,2,4,2,0,0,0,1,4,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1)
a1 <- 3
a2 <- 1
b1 <- 0.5
b2 <- 0.5

# U pdf dokumentu su sva izvodjenja raspodela

# Kao uslovne aposteriorne raspodele za lambda1 i lambda2 dobijamo :
# lambd1 ima gama raspodelu sa parametrima : a1+sum_1^m y_i i  b1+m
# lambd2 ima gama raspodelu sa parametrima : a2+sum_{m+1}^n y_i, b1+n-m
# Za pi(m|y, lambda1, lambda2) dobijemo da je nesto proporcionalno sledecem izrazu:
# exp((lambda2-lambda1)*m)*(lambda1/lambda2)^(sum_1^m y_i)

MC <- 1000 # broj iteracija
N <- 200 # duzina lanca

n <- length(Y) # obim uzorka
m <- n # nepokretna tacka 
p <- rep(0,3*MC*N) # pravimo niz u kome cemo cuvati rezultate
dim(p)<-c(3,MC,N)

for (j in (1:MC))
{
  m <- as.integer(n*runif(1))+1
  for (i in (1:N))
  {
    lambda1 <- rgamma(1,a1+sum(Y[1:m]),m+b1) 
    lambda2 <- rgamma(1,a2+sum(Y)-sum(Y[1:m]),n-m+b2)
    # za dobijanje funkcije raspodele koristimo funkciju cumsum
    pm <- exp((lambda2-lambda1)*(1:n))*(lambda1/lambda2)^cumsum(Y) 
    # sumiramo i normalizujemo da dobijemo pravu raspodelu tj. da u zbiru bude 1
    pm <- pm/sum(pm) 
    m <- min((1:n)[runif(1)<cumsum(pm)]) 
    # m uzorkujemo kao najmanji broj iz skupa dopustivih vrednosti za m, za koji dobijena funkcija raspodele,
    # prelazi slucajno izabrani broj iz (0,1)
    # cuvamo rezultate
    p[1,j,i]<-m
    p[2,j,i]<-lambda1
    p[3,j,i]<-lambda2
  }
}
  
parametar_m <-p[1,,]
parametar_lambda1 <-p[2,,]
parametar_lambda2 <-p[3,,]

# Ocenjujemo parametre aposteriornih raspodjela za lambda1 i lambda 2 metodom momenata funkcijom gamaMM
gamaMM <- function(x)
{
  n <- length(x) # duzina x
  mean_x <- mean(x) # x srednja vrednost
  alpha <- n*(mean_x^2)/sum((x-mean_x)^2) # ocena metodom momenata za alfa
  beta <- 1/(sum((x-mean_x)^2)/n/mean_x) # ocena metodom momenata za beta
  estimate <- data.frame(alpha,beta) # spojimo dve ocene u data frame
  return(estimate)
}
parametar_lambda1_MM<-gamaMM(parametar_lambda1) #parametri za aposteriornu raspodelu za lambda1
parametar_lambda2_MM<-gamaMM(parametar_lambda2) #parametri za aposteriornu raspodelu za lambda2  


x<-seq(0,1,length=1000) # delimo interval zbog fukcije raspodjele
# pravimo histogram za uzorak iz aposteriorne raspodjele za lambda1 i na njemu crtamo funkciju raspodele 
# sa ocenjenim parametrima 

hist(parametar_lambda1, main = "Histogram za parametar lambda1",xlab = "lambda1",probability = T,breaks = N)
curve(dgamma(x, parametar_lambda1_MM$alpha, parametar_lambda1_MM$beta),xlim = c(0,10),add=TRUE, col='red')

# isto to radimo i sa aposteriornom raspodelom za lambda2
hist(parametar_lambda2,main = "Histogram za parametar lambda2",xlab = "lambda2",probability = T,breaks = N)
curve(dgamma(x, parametar_lambda2_MM$alpha, parametar_lambda2_MM$beta),xlim = c(0,10),add=TRUE, col='blue')
# u oba slucaja se poklapaju funkcija raspodele i histogrami dobijenih uzoraka

hist(parametar_m, main = "Histogram za parametar m",xlab = "m",probability = T,breaks = N)
# histogram za uzorak iz aposteriorne raspodjele za m, koji ne lici ni na jednu poznatu raspodelu
