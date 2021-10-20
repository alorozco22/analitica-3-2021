###################################################
# Caso de estudio - Analítica prescriptiva
# Taller: replicando el script de Forian Teschner en https://flovv.github.io/From_descritpive_to_prescriptive/
# con datos de Kaggle en: https://www.kaggle.com/abdulmajeed33/footballsoccer-players-market-value
# 
# Para el curso Analítica prescriptiva e IA
# Educación Continua | Universidad de los Andes
# 20 de octubre 2021
###################################################

################################
# CONFIGURACIONES INICIALES
################################
# Asignar el directorio de trabajo donde R encontrará los archivos
setwd("/Users/alorozco22/OneDrive - Universidad de los Andes/Documentos 2021-20/2021 08 30 EdCo Curso Capstone IA prescriptiva/01 clases/")

# Cargamos datos desde el archivo csv
datos <- read.csv(file = '2021 10 20 Clase 4-mostvaluableplayers.csv')

# Visualizar el DataFrame haciendo clic en el panel visualizador de objetos a la derecha
View(datos)

# Para este ejercicio no contamos con ranking juego en los datos
# Normalmente podríamos construir un ranking de desempeño a mano o con datos pre-hechos
# Para hoy vamos a escoger un objetivo prescriptivo diferente: 
# Vamos a configurar el equipo con la menor edad agregada de jugadores (simplificador pero práctico para la clase)

# Este va a ser un problema de minimización, en lugar de maximización

# ETAPAS:
# 1. Plantear información para definir el problema de optimización
# 2. Plantear información para definir las restricciones del problema de optimización
# 3. Resolver el problema mediante programación lineal
# 4. Visualizar los resultados

################################
# UN MUY BREVE OJO DESCRIPTIVO
################################

# Algunas medias
sapply(datos, mean, na.rm=TRUE)

# Primero visualicemos las variables un poco, observemos una correlación
cor(datos$age, datos$price_Mill._.)
# La correlación MUY baja: 0.3%!!! Parece que la edad no es buen indicador de valor de mercado


# GRÁFICA DE DISPERSIÓN ENTRE EDAD Y PRECIO DE MERCADO
# Echemos un ojo a los datos con un diagrama de dispersión
par(xpd=TRUE) # Para que nos deje dibujar leyenda fuera del cuadro
plot(datos$age, datos$price_Mill._., col = factor(datos$position), pch = 19, xlim=c(18,48))
# Agreguemos la leyenda para los colores
legend(36, 250,
       legend = levels(factor(datos$position)),
       pch = 19,
       col = factor(levels(factor(datos$position))))

# En efecto parece no haber correlación.
# Pero además se nota que los mayores valores de mercado están más cerca a los 27 años
# Deja ver que existe una disyuntiva: experiencia, edad
# intuitivamente nos deja ver que tal vez edad es un objetivo imperfecto de optimización

##################################
# UNA TRANSFORMACIÓN DE LOS DATOS
# (muy poco conocedora)
##################################
condicion = datos$position == "Attacking Midfield"
condicion # Un vector/columna de verdaderos y falsos
datos$position[condicion] <- "Ofensa"

datos$position[datos$position == "Central Midfield"] <- "Mediocampista"
datos$position[datos$position == "Centre-Back"] <- "Mediocampista"
datos$position[datos$position == "Centre-Forward"] <- "Ofensa"
datos$position[datos$position == "Defensive Midfield"] <- "Defensa"
datos$position[datos$position == "Goalkeeper"] <- "Arquero"
datos$position[datos$position == "Left Midfield"] <- "Mediocampista"
datos$position[datos$position == "Left Winger"] <- "Mediocampista"
datos$position[datos$position == "Left-Back"] <- "Mediocampista"
datos$position[datos$position == "Right Midfield"] <- "Mediocampista"
datos$position[datos$position == "Right Winger"] <- "Mediocampista"
datos$position[datos$position == "Right Back"] <- "Mediocampista"
datos$position[datos$position == "Second Striker"] <- "Ofensa"
datos$position[datos$position == "Right-Back"] <- "Mediocampista"

# Volvamos a generar la gráfica para revisar

################################
# ANÁLISIS PRESCRIPTIVO
################################

# Como vamos a usar programación lineal, fijémonos en qué necesitamos:
?lp

# 1. Plantear información para definir el problema de optimización
#install.packages("lpSolve") # Instalamos el paquete de programación lineal desde CRAN por si no está ya
library(lpSolve) # Importamos el paquete de programación lineal

f.obj <- datos$age ### la columna objetivo! 



# 2. Plantear información para definir las restricciones del problema de optimización
#presupuesto 20 mill, 13 jugadores, 1 portero, 5 defensas, 5 mediocampista, 3 goleadores 
f.rhs <- c(600, 13, 1, 5, 5, 3) 
# el presupuesto correspondiente debe ser menor que los definidos arriba, 
# exactamente un portero
f.dir <- c("<=", "=", "=", "<=", "<=" ,"<=")

f.con <- t(datos$price_Mill._.) ### Restricción: máximo valor <= presupuesto
player <- rep(1, nrow(datos)) ## El resultado va a ser una columna de jugadores (inicializamos a todos en 1)
f.con <- rbind(f.con, player) # A la columna de condiciones le pegamos la de valor de mercado y la de jugador 

## restricción: por posición puede haber solo un cierto número de jugadores
## (ej. Sólo un portero) 
## Definimos matriz – para los 500 jugadores me indica en qué posición juega
A <- as.data.frame(model.matrix(price_Mill._. ~ position -1, datos) )
f.con <- rbind(f.con, t(as.matrix(A))) # A las condiciones le vamos pegando la info de posición



# 3. Resolver el problema mediante programación lineal
solved <- lp("min", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE) ## la solución debe quedar como variables de decisión binarias!

################### salida! 
datos$buy <- solved$solution



# 4. Visualizar los resultados
sum(datos[datos$buy == 1,]$price_Mill._.) ## de cuánto quedó el presupuesto
comprados <- sum(datos[datos$buy == 1,]$buy)   ## número de jugadores comprados
comprados
edad <- sum(datos[datos$buy == 1,]$age) ## cuál es la edad agregada del equipo
edad
#Eso es en promedio por jugador:
edad/comprados
paste(datos[datos$buy == 1,]$name, collapse=", ") # Nombre de jugadores comprados


################################
# ACTIVIDAD
################################

# Ejercicio: ¿De qué países son los jugadores comprados?


# Ejercicio: ¿Qué pasa si bajamos la restricción presupuestal a 400 millones? ¿A 200?


# Ejercicio: ¿Cuál es el resultado si queremos el equipo con la mayor edad total posible (no a menor)?
