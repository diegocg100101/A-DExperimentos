datos1 = 
datos2 = 

x_1 = mean(datos1)
x_2 = mean(datos2)

s_1 = var(datos1)
s_2 = var(datos2)

n_1 = length(datos1)
n_2 = length(datos2)

alpha = 
alpha2 = alpha / 2

Z = -qnorm(alpha)
Z2 = -qnorm(alpha2)

Z_test = (x_1 - x_2) / sqrt( s_1 / n_1 + s_2 / n_2)

if(Z_test > Z2 | Z_test < -Z2){
    cat( "Dato 1 !=  Dato 2\n")

    # Verifica si entra en la región de rechazo (Hipótesis alterna)
    if(Z_test > Z){
        cat("Dato 1 > Dato 2\n")
    } else if (Z_test < -Z) {
        cat("Dato 1 < Dato 2\n")
    }
    } else {
        cat("Dato 1 = Dato 2\n")
    }

