g1 = c(8,9,6,7,3)
g2 = c(2,4,3,5,1)
g3 = c(3,5,4,2,3)
g4 = c(2,2,-1,0,3)

x1 = mean(g1) 
x2 = mean(g2)
x3 = mean(g3)
x4 = mean(g4)

K = 4 # Número total de grupos
 
N = length(g1) + length(g2) + length(g3) + length(g4) # Número de muestras totales

X = sum(g1, g2, g3, g4) / N   

df1 = K - 1
df2 = N - K

n = length(g1) # Número de muestras por grupo

SSB = n * ((x1 - X)^2 + (x2 - X)^2 + (x3 - X)^2 + (x4 - X)^2)

sum_g1 = sum((g1 - x1)^2)
sum_g2 = sum((g2 - x2)^2)
sum_g3 = sum((g3 - x3)^2)
sum_g4 = sum((g4 - x4)^2)

SSE = sum_g1 + sum_g2 + sum_g3 + sum_g4

sst_g1 = sum((g1 - X)^2)
sst_g2 = sum((g2 - X)^2)
sst_g3 = sum((g3 - X)^2)
sst_g4 = sum((g4 - X)^2)

SST = sst_g1 + sst_g2 + sst_g3 + sst_g4

MSB = SSB / df1
MSE = SSE / df2

F = MSB / MSE