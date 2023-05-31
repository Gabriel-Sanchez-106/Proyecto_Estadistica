

# Función para simular un proceso de Poisson homogéneo
simulate_Poisson <- function(lambda, T) {
  # Generar el número de eventos en el intervalo [0, T]
  N <- rpois(1, lambda * T)
  
  # Generar N tiempos de llegada uniformemente distribuidos en (0, T)
  arrival_times <- runif(N, 0, T)
  
  # Ordenar los tiempos de llegada de menor a mayor
  arrival_times <- sort(arrival_times)
  
  return(arrival_times)
}


# Función para realizar la prueba de bondad de ajuste utilizando el estadístico de Pearson
pearson_uniform_test <- function(data) {
  observed_data<-count_values_in_uniform_intervals(data, length(data))
  # Se define la probabilidad esperada
  expected_probabilities <- rep(1/length(observed_data), length(observed_data))
  
  # Se define la frecuencia esperada
  expected_frequencies <- expected_probabilities * sum(observed_data)
  
  # Se calcula el estadistico de pearson
  Pearson_statistic <- sum((observed_data - expected_frequencies)^2 / expected_frequencies)
  
  # Grados de libertad
  df <- length(observed_data) - 1
  
  # Calcula p-valor
  p_value <- pchisq(Pearson_statistic, df=df, lower.tail = FALSE)
  return(p_value)
}

count_values_in_uniform_intervals <- function(sample, n) {
  # Calculamos el ancho del intervalo
  max_value <- max(sample)
  interval_width <- max_value / n
  # Vector de conteo
  counts <- rep(0, n)
  # Valores por intervalo
  for (i in 1:n) {
    lower_bound <- (i-1) * interval_width
    upper_bound <- i * interval_width
    if (i == n) {
      counts[i] <- sum(sample >= lower_bound & sample <= max_value)
    } else {
      counts[i] <- sum(sample >= lower_bound & sample < upper_bound)
    }
  }
  # Retorna los conteos observados
  return(counts)
}

# Función para realizar una prueba suave de bondad de ajuste
smooth_goodness_of_fit_test <- function(data) {
    # Calcula el test de Kolmógorov-Smirnov
    test_result <- ks.test(data, "punif", 0, max(data))
    
    # Obtiene el p-valor
    return(test_result$p.value)
}

# Función para realizar simular un proceso poisson no homogeneo
poisson_nonhomogeneous_intensity_A <- function(T) {
  # Paso I: Calcular el valor máximo de la función de intensidad
  lambda_max <- max(sapply(seq(0, T, length.out = 1000), lambda_t_A))
  
  # Paso II: Generar una variable Poisson con media T * lambda_max
  N <- rpois(1, T * lambda_max)
  
  # Paso III: Generar una muestra aleatoria uniforme (0, T)
  u <- runif(N, 0, T)
  
  # Paso IV: Generar una muestra aleatoria uniforme (0, lambda_max)
  h <- runif(N, 0, lambda_max)
  
  # Paso V: Filtrar los eventos según la condición hi < lambda(ui)
  selected_events <- u[h < lambda_t_A(u)]
  
  # Paso VI: Ordenar los eventos seleccionados
  selected_events <- sort(selected_events)
  
  return(selected_events)
}

poisson_nonhomogeneous_intensity_B <- function(T) {
  ## Paso I: Calcular lambda*
  lambda_max <- 1.2
  
  # Paso II: Generar variable de Poisson
  N <- rpois(1, T * lambda_max)
  
  # Paso III: Generar muestra aleatoria uniforme
  u <- runif(N, min = 0, max = T)
  
  # Paso IV: Generar muestra aleatoria uniforme
  h <- runif(N, min = 0, max = lambda_max)
  
  # Paso V: Seleccionar datos
  selected <- u[h < sapply(u, function(t) lambda_t_B(t))]
  
  # Paso VI: Ordenar los datos seleccionados
  sorted <- sort(selected)
  
  return(sorted)
}

# Intensidades
lambda_t_A <- function(t) {
  return(1 + 0.02 * t)
}

lambda_t_B <- function(t) {
  if ((t >= 0 && t < 20) || (t >= 40 && t < 60) || (t >= 80 && t < 100)) {
    return(1)
  } else if ((t >= 20 && t < 40) || (t >= 60 && t < 80)) {
    return(1.2)
  } else {
    return(0)
  }
}

# Funcion para hacer la prueba de hipótesis exp
exponential_hypothesis_test <- function(X, Y) {
  X <- diff(c(0, X))
  Y <- diff(c(0, Y))
  mean_X <- mean(X)
  mean_Y <- mean(Y)
  
  test_statistic <- mean_Y / mean_X
  
  n <- length(Y)
  m <- length(X)
  
  df1 <- n
  df2 <- m
  
  p_value <- 1 - pf(test_statistic, df1, df2, lower.tail = TRUE)
  
  print(p_value)
}

# Funcion para hacer la prueba de hipótesis a1=a2 inciso B
binomial_hypothesis_test <- function(X,Y){
  n <- length(Y) #Se toma la longitud
  m <- length(X)
  
  s <- sum(X)
  c <- round(s+sum(Y)) #Se calcula "y"
  p_value <- pbinom(s, size= c , prob= m/(m+n) , lower.tail = FALSE) #Se calcula el p-valor usando Bin

  print(p_value)
}


T1=20
T2=50
T3=100

XH=simulate_Poisson(1,T3)
print(smooth_goodness_of_fit_test(XH))
print(pearson_uniform_test(XH))

power_of_test<-function(){
  Ks=0
  Kp=0
  for (i in (1:3000)){
    XNHA1= poisson_nonhomogeneous_intensity_B(T3)
    sa1=smooth_goodness_of_fit_test(XNHA1)
  
    p1=pearson_uniform_test(XNHA1)
  
    if(sa1>=0.05){Ks=Ks+1}
    if(p1>=0.5){Kp=Kp+1}
  }
  print((1-Ks/3000)*100)
  print((1-Kp/3000)*100)
}



