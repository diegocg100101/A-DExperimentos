g11 = c(130, 155, 74, 180)
g12 = c(34, 40, 80, 75)
g13 = c(20, 70, 82, 58)
g21 = c(150, 188, 159, 126)
g22 = c(136, 122, 106, 115)
g23 = c(25, 70, 58, 45)
g31 = c(138, 110, 168, 160)
g32 = c(174, 120, 150, 139) 
g33 = c(96, 104, 82, 60)

y = sum(c(g11, g12, g13, g21, g22, g23, g31, g32, g33))

y_2 = sum(c(g11, g12, g13, g21, g22, g23, g31, g32, g33)^2)

a = 3 # filas
b = 3 # columnas
n = 4 # repeticiones por casilla

y_abn = (y ^ 2)/(a * b * n)

SST = y_2 - y_abn

y_i = (sum(g11, g12, g13)^2 + sum(g21, g22, g23)^2 + sum(g31, g32, g33)^2) / (b * n)
 
SSA = y_i - y_abn

y_j = (sum(g11, g21, g31)^2 + sum(g12, g22, g32)^2 + sum(g13, g23, g33)^2) / (a * n)

SSB = y_j - y_abn

y_ij = sum(c(sum(g11)^2, sum(g12)^2, sum(g13)^2, sum(g21)^2, sum(g22)^2, sum(g23)^2, sum(g31)^2, sum(g32)^2, sum(g33)^2)) / n

SSsub = y_ij - y_abn

SSAB = SSsub - SSA - SSB

SSE = SST - SSsub

dfA = a - 1 
dfB = b - 1
dfAB = dfA * dfB
dfE = a * b * (n - 1)
dfT = a * b * n - 1

MSA = SSA / dfA
MSB = SSB / dfB
MSAB = SSAB / dfAB
MSE = SSE / dfE

F_A = MSA / MSE
F_B = MSB / MSE
F_AB = MSAB / MSE

cat("SSA: ", SSA,"\n")
cat("SSB: ", SSB,"\n")
cat("SSAB: ", SSAB,"\n")
cat("SSE: ", SSE,"\n")
cat("SST: ", SST,"\n")
cat("----------------\n")
cat("df_A: ", dfA,"\n")
cat("df_B: ", dfB,"\n")
cat("df_AB: ", dfAB,"\n")
cat("df_E: ", dfE,"\n")
cat("df_T: ", dfT,"\n")
cat("----------------\n")
cat("F_A: ", F_A,"\n")
cat("F_B: ", F_B,"\n")
cat("F_AB: ", F_AB,"\n")