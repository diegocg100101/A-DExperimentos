# Grupos (cambiar)
g1 = c(65,87,73,79,81,69)
g2 = c(75,69,83,81,72,79,90)
g3 = c(59,78,67,62,83,76)
g4 = c(94,89,80,88)

# Promedios (cambiar)
x1 = mean(g1) 
x2 = mean(g2)
x3 = mean(g3) 
x4 = mean(g4)

# Número total de grupos (cambiar)
K = 4
 
# Número de grupos (cambiar)
N = length(g1) + length(g2) + length(g3) + length(g4) 

# Número total
T = N - 1

# Promedio total (cambiar)
X = sum(g1, g2, g3, g4) / N  

# Desviaciones estándar
s1 = sqrt(var(g1))
s2 = sqrt(var(g2))
s3 = sqrt(var(g3))
s4 = sqrt(var(g4))

# Tamaños de muestras (cambiar)
n1 = length(g1) # Tamaño de un grupo
n2 = length(g2)
n3 = length(g3)
n4 = length(g4)

df1 = K - 1 # Numerador
df2 = N - K # Denominador

# Calular SSB (cambiar)
ssb_g1 = n1 * (x1 - X)^2
ssb_g2 = n2 * (x2 - X)^2
ssb_g3 = n3 * (x3 - X)^2
ssb_g4 = n4 * (x4 - X)^2

SSB = ssb_g1 + ssb_g2 + ssb_g3 + ssb_g4

# Calular SSE (cambiar)
sum_g1 = sum((g1 - x1)^2)
sum_g2 = sum((g2 - x2)^2)
sum_g3 = sum((g3 - x3)^2)
sum_g4 = sum((g4 - x4)^2)

SSE = sum_g1 + sum_g2 + sum_g3 + sum_g4 

# Calular SST (cambiar)
sst_g1 = sum((g1 - X)^2)
sst_g2 = sum((g2 - X)^2)
sst_g3 = sum((g3 - X)^2)
sst_g4 = sum((g4 - X)^2)

SST = sst_g1 + sst_g2 + sst_g3 + sst_g4 

# Calcular MSB
MSB = SSB / df1

# Calcular MSE
MSE = SSE / df2

F = MSB / MSE # F_test