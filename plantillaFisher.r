# Grupos (cambiar)
g1 = c(30.99, 29.72, 31.30, 33.05, 29.53, 30.57, 29.67)
g2 = c(28.15, 26.30, 27.81, 26.30, 26.30, 27.29, 28.29, 26.30)
g3 = c(21.33, 20.78, 22.38, 20.91, 20.31, 21.47, 21.17, 21.51, 21.18)
g4 = c(34.02, 35.20, 32.93, 35.68, 33.92, 35.67, 35.67)
g5 = c(27.78, 24.98, 23.41, 26.23, 23.17, 23.91, 26.45, 24.81, 26.42)
g6 = c(20.74, 20.17, 19.88, 19.70, 18.52, 19.83, 20.17, 19.88, 20.02, 20.29)

# Significancia
alpha = 0.01

# Promedios (cambiar)
x1 = mean(g1) 
x2 = mean(g2)
x3 = mean(g3) 
x4 = mean(g4)
x5 = mean(g5)
x6 = mean(g6)

# Número total de grupos (cambiar)
K = 6
 
# Número de grupos (cambiar)
N = length(g1) + length(g2) + length(g3) + length(g4) + length(g5) + length(g6)

# Número total
T = N - 1

# Promedio total (cambiar)
X = sum(g1, g2, g3, g4, g5, g6) / N  

# Desviaciones estándar
s1 = sqrt(var(g1))
s2 = sqrt(var(g2))
s3 = sqrt(var(g3))
s4 = sqrt(var(g4))
s5 = sqrt(var(g5))
s6 = sqrt(var(g6))

# Tamaños de muestras (cambiar)
n1 = length(g1) # Tamaño de un grupo
n2 = length(g2)
n3 = length(g3)
n4 = length(g4)
n5 = length(g5)
n6 = length(g6)

df1 = K - 1 # Numerador
df2 = N - K # Denominador

# Calular SSB (cambiar)
ssb_g1 = n1 * (x1 - X)^2
ssb_g2 = n2 * (x2 - X)^2
ssb_g3 = n3 * (x3 - X)^2
ssb_g4 = n4 * (x4 - X)^2
ssb_g5 = n5 * (x5 - X)^2
ssb_g6 = n6 * (x6 - X)^2

SSB = ssb_g1 + ssb_g2 + ssb_g3 + ssb_g4 + ssb_g5 + ssb_g6

# Calular SSE (cambiar)
sum_g1 = sum((g1 - x1)^2)
sum_g2 = sum((g2 - x2)^2)
sum_g3 = sum((g3 - x3)^2)
sum_g4 = sum((g4 - x4)^2)
sum_g5 = sum((g5 - x5)^2)
sum_g6 = sum((g6 - x6)^2)

SSE = sum_g1 + sum_g2 + sum_g3 + sum_g4 + sum_g5 + sum_g6

# Calular SST (cambiar)
sst_g1 = sum((g1 - X)^2)
sst_g2 = sum((g2 - X)^2)
sst_g3 = sum((g3 - X)^2)
sst_g4 = sum((g4 - X)^2)
sst_g5 = sum((g5 - X)^2)
sst_g6 = sum((g6 - X)^2)

SST = sst_g1 + sst_g2 + sst_g3 + sst_g4 + sst_g5 + sst_g6

# Calcular MSB
MSB = SSB / df1

# Calcular MSE
MSE = SSE / df2

# F_test
F = MSB / MSE 

# F crítico
F_critico = qf(1 - alpha, df1, df2)

cat("F = " , F , "\n")
cat("F_critico = " , F_critico , "\n")
cat("SSB = " , SSB , "\n")
cat("SSE = " , SSE , "\n")
cat("SST = " , SST , "\n")
cat("MSB = " , MSB , "\n")
cat("MSE = " , MSE , "\n")

if(F > F_critico){
    cat("Dentro de RR\n")
} else {
   cat("Fuera de RR\n")
}