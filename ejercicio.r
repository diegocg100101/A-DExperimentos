# Otra forma
# female = read.table('female.txt')
# mean(female$V1)

# Leer archivos en R
male <- scan('male.txt', what=numeric())
female <- scan('female.txt', what=numeric())

promedio_chicos = mean(male)
promedio_chicas = mean(female)

cat("Promedio chicos: ", promedio_chicos, "\n")
cat("Promedio chicas: ", promedio_chicas, "\n")

# hist(male, main="Male")
# hist(female, main="Female")



