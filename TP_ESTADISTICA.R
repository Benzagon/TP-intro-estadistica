################## FUNCIONES AUXILIARES ##################

# Calcula el peso de una persona ponderando si es adulto o niño.
peso_general_una_persona <- function(proba_adulto) {
  es_adulto <- rbinom(1,1,proba_adulto) # La persona es adulto o no
  if(es_adulto) {
    return(rnorm(1, 70, 7))
  }
  return(rnorm(1, 20, 5))
}

# Simula 1 vuelo con 'pasajeros' y proba de ser adulto. Devuelve verdadero si no es óptimo.
simular_vuelo_no_optimo_con_pasajeros <- function(pasajeros, p_adulto) {
  pesos_pasajeros <- replicate(pasajeros, peso_general_una_persona(p_adulto))
  return(sum(pesos_pasajeros) > PESO_LIMITE)
}

# Simula 'reps' vuelos por cada 'p_adulto'. 'lambda' determina cuantos pasajeros llegan (mediante una poisson).
simular_vuelos <- function(reps, p_adulto, lambda) {
  suma_de_probas <- 0
  for (i in 1:reps) {
    pasajeros <- rpois(1, lambda)
    if (pasajeros > 81) { # Si no entran en el avión.
      pasajeros <- 81
    }
    suma_de_probas <- (suma_de_probas + 1 * simular_vuelo_no_optimo_con_pasajeros(pasajeros, p_adulto))
  }
  return(suma_de_probas / reps);
}

############################################################

###########################
####### ESCENARIO 1 #######
###########################

# Variables y constantes
REPS <- 1000
PESO_LIMITE <- 5500
suma_de_probas <- 0

set.seed(0)

# Simula 1 vuelo con 81 personas. Devuelve verdadero si no es óptimo.
simular_vuelo_no_optimo <- function() {
  pesos_pasajeros <- rnorm(81, 70, 7)
  return(sum(pesos_pasajeros) > PESO_LIMITE)
}

# Simulamos REPS veces.
for (i in 1:REPS) {
  suma_de_probas <- (suma_de_probas + 1 * simular_vuelo_no_optimo())
}

# Casos favorables sobre casos totales.
print(paste("RTA ESCENARIO 1:",suma_de_probas / REPS, sep = " "))

###########################
####### ESCENARIO 2 #######
###########################

# Variables y constantes
REPS <- 1000
PESO_LIMITE <- 5500
P_ADULTO <- 0.95
suma_de_probas <- 0

set.seed(0)

# Simulamos REPS veces.
for (i in 1:REPS) {
  suma_de_probas <- (suma_de_probas + 1 * simular_vuelo_no_optimo_con_pasajeros(81, P_ADULTO)) # Simula 1 vuelo con 81 pasajeros ponderando si son niños.
}

# Casos favorables sobre casos totales.
print(paste("RTA ESCENARIO 2:",suma_de_probas / REPS, sep = " "))

###########################
####### ESCENARIO 3 #######
###########################

# Variables y constantes
REPS <- 1000
PESO_LIMITE <- 5500
P_ADULTO <- seq(0.9, 1, by = 0.01) # Secuencia de probas de ser adulto

set.seed(0)

### INCISO 1 ###

# Simulamos 1000 vuelos por cada P_ADULTO. Lambda está fijo en 70.
simular_por_p_adulto <- sapply(P_ADULTO, function(p) simular_vuelos(REPS, p, 70))

# Graficar
plot(P_ADULTO, simular_por_p_adulto, type = "l", 
     xlab = "Probabilidad de adulto (P_ADULTO)",
     ylab = "Proba de vuelo no óptimo",
     main = "Simulación según P_ADULTO",
     col = "blue", lwd = 2)

### INCISO 2 ###

LAMBDAS <- seq(70, 100, by = 5) # Secuencia de lambdas.

# Simulamos 1000 vuelos por cada LAMBDAS y P_ADULTO.
simular_por_p_adulto <- simplify2array(
  lapply(LAMBDAS, function(l) {
    sapply(P_ADULTO, function(p) simular_vuelos(REPS, p, l))
  })
)

colores <- rainbow(length(LAMBDAS)) # Colores para las curvas

# Graficar todas las columnas de simular_por_p_adulto en una sola llamada
matplot(P_ADULTO, simular_por_p_adulto, type = "p", col = colores, pch = 16,
        ylim = range(simular_por_p_adulto),
        xlab = "Probabilidad de adulto (P_ADULTO)",
        ylab = "Probabilidad vuelo no óptimo",
        main = "Simulación vuelo no óptimo para distintos λ")

# Agregar leyenda
legend("topleft", legend = paste("λ =", LAMBDAS),
       col = colores, pch = 16, cex = 1)