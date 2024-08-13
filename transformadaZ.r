# Leer archivos en R
male = scan('male.txt', what=numeric())
female = scan('female.txt', what=numeric())

# Transformada Z
# Z = (x_1 - x_2) / sqrt(s_1²/n_1 + s_2²/n_2)
# s - varianza
# n - cantidad 

x_1 = mean(male)
x_2 = mean(female)

s_1 = var(male)
s_2 = var(female)

n_1 = length(male)
n_2 = length(female)

# Trasnformada Z
# Z de prueba
Z_test = (x_1 - x_2) / sqrt( s_1 / n_1 + s_2 / n_1)