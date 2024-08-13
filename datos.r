datos = load("concrete.RData")

# Inicializa vectores
nombres = c()
x_todos = c()
s_todos = c()
n_total = c()


# Crea vectores coordinados para procesar los datos
for(dato in datos){
    if(dato != ".Random.seed"){
        nombres = c(nombres, dato) # Guarda los nombres de los datos
        dato = get(dato)
        x_todos = c(x_todos, mean(dato)) # Guarda promedios
        s_todos = c(s_todos, var(dato)) # Guarda varianzas
        n_total = c(n_total, length(dato)) # Guarda tamaños de muestra
    }
}

# Alpha 
alpha = 0.01
alpha2 = alpha / 2 # Alpha para región de rechazo

# Medir área de Z (Región de rechazo)
Z = 1 - pnorm(alpha) # Z_a
Z2 = 1 - pnorm(alpha2) # Z_a/2

# Prueba cada par de datos posibles
for(i in 1:5){
    for(j in i+1:5){
        if(j <= 6){

            # Calcula Z de prueba
            Z_test = (x_todos[i] - x_todos[j]) / sqrt( s_todos[i] / n_total[i] + s_todos[j] / n_total[j])

            # Verifica si entra en la región de rechazo (Hipótesis nula)
            if(Z_test > Z2 | Z_test < -Z2){
                cat(nombres[i], " != ", nombres[j], "\n")

                # Verifica si entra en la región de rechazo (Hipótesis alterna)
                if(Z_test > Z){
                    cat(nombres[i], " > ", nombres[j], "\n")
                } else if (Z_test < -Z) {
                    cat(nombres[i], " < ", nombres[j], "\n")
                }

                #if(x_todos[i] > x_todos[j]){
                #    cat(nombres[i], " > ", nombres[j], "\n")
                #} else {
                #    cat(nombres[i], " < ", nombres[j], "\n")
                #}

            } else {
               cat(nombres[i], " = ", nombres[j], "\n")
            }
        }
    }    
}