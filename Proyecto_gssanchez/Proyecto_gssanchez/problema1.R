## Problema 1

d=0.3
n=100
m=100

X1=rlaplace(n,0, 1)
Y1=d+rlaplace(m, 0, 1)

X2_2=rt(n,2)
Y2_2=d+rt(m,2)

X2_10=rt(n,10)
Y2_10=d+rt(m,10)

X2_50=rt(n,50)
Y2_50=d+rt(m,50)

## Punto 1

bt<-function(x, y , b){
  
  n1=length(x) #Obtenemos las longitudes
  n2=length(y)
  v=mean(y)-mean(x) #Se define el valor crítico de conteo
  
  z=c(x,y) #Unificamos las muestras
  
  counter=0 #Inicializamos el contador
  
  for(i in 1:b){
    
    xstar<-sample(z,n1,replace=T) #Creamos nuevas permutaciones
    ystar<-sample(z,n2,replace=T)
    
    vstar<-mean(ystar)-mean(xstar) #Se calcula su valor
    
    if(vstar>=v) {counter=counter+1} #Se añade al contador si es valido
  }
  pvalue=counter/b #Se calcula el pvalor
  print(pvalue)
  
}

## Punto 2

wc<-function(x,y){
  ans=wilcox.test(x,y)
  print(ans$p.value)
}

## Punto 3

tt<-function(x,y){
  n_x <- length(x) #Obtenemos longitudes
  n_y <- length(y)
  
  mean_x <- mean(x) #Calculamos medias muestrales
  mean_y <- mean(y)
  
  var_x <- var(x) #Se calcula la varianza muestral
  var_y <- var(y)
  
  pooled_var <- ((n_x - 1) * var_x + (n_y - 1) * var_y) / (n_x + n_y - 2) #Sp^2
  t_value <- (mean_x - mean_y) / sqrt(pooled_var * (1/n_x + 1/n_y)) #Se calcula T
  
  df <- n_x + n_y - 2 #Se establecen los grados de libertad
  p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE) #Calculamos p-valor
  
  print(p_value)
}

## Amazon

data <- read.csv("AMZN.csv", sep=";")

C21 <- data$X2021 # Info 2021

C22 <- data$X2022 # Info 2022
C22 <- C22[-length(C22)]

bt(C22,C21,15000)


