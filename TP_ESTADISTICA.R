#### ESCENARIO 1
# Variables
REPS <- 1000
PESO_LIMITE <- 5500
suma_de_probas <- 0

set.seed(0)

# Simula 1 vuelo. Devuelve verdadero si no es óptimo.
simular_vuelo_no_optimo <- function() {
  pesos_pasajeros <- rnorm(81, 70, 7)
  return(sum(pesos_pasajeros) > PESO_LIMITE)
}

# Simulamos REPS veces.
for (i in 1:REPS) {
  suma_de_probas <- (suma_de_probas + 1 * simular_vuelo_no_optimo())
}

# Casos favorables sobre casos totales.
print(suma_de_probas / REPS)

### ESCENARIO 2
REPS <- 1000
PESO_LIMITE <- 5500
P_ADULTO <- 0.95
suma_de_probas <- 0

set.seed(0)

# Calcula el peso de una persona ponderando si es adulto o niño.
peso_general_una_persona <- function(proba_adulto) {
  es_adulto <- rbinom(1,1,proba_adulto) # La persona es adulto o no
  if(es_adulto) {
    return(rnorm(1, 70, 7))
  }
  return(rnorm(1, 20, 5))
}

# Simula 1 vuelo. Devuelve verdadero si no es óptimo.
simular_vuelo_no_optimo_con_ninos <- function() {
  pesos_pasajeros <- replicate(81, peso_general_una_persona(P_ADULTO))
  return(sum(pesos_pasajeros) > PESO_LIMITE)
}

# Simulamos REPS veces.
for (i in 1:REPS) {
  suma_de_probas <- (suma_de_probas + 1 * simular_vuelo_no_optimo_con_ninos())
}

# Casos favorables sobre casos totales.
print(suma_de_probas / REPS)

### ESCENARIO 3
REPS <- 1000
PESO_LIMITE <- 5500
P_ADULTO <- seq(0.9, 1, by = 0.01)

set.seed(0)

# Simula 1 vuelo con 'pasajeros' y proba de ser adulto. Devuelve verdadero si no es óptimo.
simular_vuelo_no_optimo_con_pasajeros <- function(pasajeros, p_adulto) {
  pesos_pasajeros <- replicate(pasajeros, peso_general_una_persona(p_adulto))
  return(sum(pesos_pasajeros) > PESO_LIMITE)
}

simular_vuelos <- function(reps, p_adulto, lambda) {
  suma_de_probas <- 0
  for (i in 1:reps) {
    pasajeros <- rpois(1, lambda)
    if (pasajeros > 81) {
      pasajeros <- 81
    }
    suma_de_probas <- (suma_de_probas + 1 * simular_vuelo_no_optimo_con_pasajeros(pasajeros, p_adulto))
  }
  return(suma_de_probas / reps);
}

# INCISO 1
simular_por_p_adulto <- sapply(P_ADULTO, function(p) simular_vuelos(REPS, p, 70))

plot(P_ADULTO, simular_por_p_adulto, type = "l",
     xlab = "Probabilidad de adulto (P_ADULTO)",
     ylab = "Proba de vuelo no óptimo",
     main = "Simulación según P_ADULTO",
     col = "blue", lwd = 2)

# INCISO 2
LAMBDAS <- seq(70, 100, by = 5)

simular_por_p_adulto <- simplify2array(
  lapply(LAMBDAS, function(l) {
    sapply(P_ADULTO, function(p) simular_vuelos(REPS, p, l))
  })
)

colores <- rainbow(length(LAMBDAS))

# Plot the first curve
plot(P_ADULTO, simular_por_p_adulto[,1], type = "l", col = colores[1],
     ylim = range(simular_por_p_adulto),
     xlab = "Probabilidad de adulto (P_ADULTO)",
     ylab = "Probabilidad vuelo no óptimo",
     main = "Simulación vuelo no óptimo para distintos λ")

# Add the other curves with lines()
for (i in 2:length(LAMBDAS)) {
  lines(P_ADULTO, simular_por_p_adulto[, i], col = colores[i])
}

# Add legend
legend("topleft", legend = paste("λ =", LAMBDAS),
       col = colores, lwd = 0.8, cex = 0.7)