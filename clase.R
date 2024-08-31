install.packages("gtools")
install.packages("combinat")

library("combinat")
library("gtools")

treatments = c("T1", "T2", "T3", "T4")

# 4! = 24 posibles combinaciones
permutations(n=4, r=4, v=treatments, repeats.allowed=F)

sample(1:24,8,replace=F)