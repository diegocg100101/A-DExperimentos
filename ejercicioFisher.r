# Grupos
g1 = c(6.1,7.1,7.8,6.9,7.6,8.2)
g2 = c(9.1,8.2,8.6,6.9,7.5,7.9)

# Promedios
x1 = mean(g1) 
x2 = mean(g2)

K = 2 # Número total de grupos
 
N = length(g1) + length(g2) # Número de grupos

X = sum(g1, g2) / N  # Promedio total

n = length(g1) # Tamaño de un grupo

df1 = K - 1 # Numerador
df2 = N - K # Denominador

SSB = n * ((x1 - X)^2 + (x2 - X)^2)

sum_g1 = sum((g1 - x1)^2)
sum_g2 = sum((g2 - x2)^2)

SSE = sum_g1 + sum_g2

sst_g1 = sum((g1 - X)^2)
sst_g2 = sum((g2 - X)^2)

SST = sst_g1 + sst_g2 

MSB = SSB / df1
MSE = SSE / df2

F = MSB / MSE # F_test