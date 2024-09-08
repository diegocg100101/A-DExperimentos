# Grupos (cambiar)
g1 = c(420, 430, 415, 425, 440, 435)
g2 = c(410, 420, 430, 425, 405, 415, 435)
g3 = c(460, 470, 455, 465, 480, 475, 460, 485)
g4 = c(390, 400, 385, 395, 410, 380)
g5 = c(430, 440, 425, 450, 435, 445, 460, 455)
g6 = c(420, 430, 425, 440, 435, 445, 455, 460, 470)

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

F = MSB / MSE # F_test