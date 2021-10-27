##############################################################
# Caso de estudio - Proceso de aprendizaje de máquinas (Con red neuronal)
# Taller: replicando el script de Dr. Bharatendra Rai 
# en: https://www.youtube.com/watch?v=-Vs9Vae2KI0&t=1218s
# Para el curso Taller comercio y desarrollo regional (Módulo Aprendizaje de máquinas)
# Educación Continua | Universidad de los Andes
# 20 de octubre 2021
##############################################################

# Etapas:
# 1. CONFIGURACIONES INICIALES
# 2. LIMPIEZA DE DATOS
# 3. ALGUNAS OBSERVACIONES DESCRIPTIVAS
# 4. SEGMENTACIÓN DE DATOS 
# 5. ENTRENAMIENTO DEL MODELO
# 6. RESULTADOS DEL ENTRENAMIENTO
# 7. EVALUACIÓN - REVISAMOS CUÁNTAS VECES HUBO ERROR EN LA PREDICCIÓN

################################
# 1. CONFIGURACIONES INICIALES
################################
# Indicamos a R dónde buscar y escribir archivos: directorio de trabajo
setwd("/Users/alorozco22/OneDrive - Universidad de los Andes/Documentos 2021-20/2021 08 30 EdCo Curso Contraloría ML Fayber/repo-comercio/data")
# Cargamos los datos a un DataFrame desde el archivo separado por comas

# Ejercicio: CARGAR LOS DATOS binary.csv con la opción header como verdadera
data <-

# Echamos un ojo a las columnas, su tipo y primeros valores
str(data)

# Visualizamos la tabla de datos
View(data)

################################
# 2. LIMPIEZA DE DATOS
################################
# Preprocesamos los datos: para redes neuronales queremos que los datos estén entre 0 y 1
# para acceder a la columna gre en el dataframe data indicamos: data$gre
# <- se utiliza para asignarle un nuevo valor
# limpiamos: calculamos (valor-mínimo)/(máximo-mínimo)
data$gre <- (data$gre - min(data$gre))/(max(data$gre) - min(data$gre))
data$gpa <- (data$gpa - min(data$gpa))/(max(data$gpa) - min(data$gpa))
# Ejercicio: convertir a entre cero y 1 la columna data$rank


# Según cada problema, limpiamos los datos como los necesitamos.
# Por lo general, les quitamos datos atípicos, revisamos que no haya erroes, etc...

#####################################
# 3. ALGUNAS OBSERVACIONES DESCRIPTIVAS
#####################################

# Podemos correr correlaciones, o visualizar datos por ejemplo
cor(data$admit, data$gre)
cor(data$admit, data$gpa)
cor(data$admit, data$rank)

# Noten, en este caso, las correlaciones están al rededor de 20% y aún así la máquina funciona

################################
# 4. SEGMENTACIÓN DE DATOS 
################################
# Como vamos a aplicar un procedimiento aleatorio, podemos indicar una semilla
# para poder repetir el procedimiento con los mismos resultados
set.seed(222) 
# con la función "sample" podemos generar muestras de los datos:
# en este caso, son dos grupos, maso el 70% de los datos en uno, y el 30% en el otro.
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
# con la función head(datos) podemos ver los 6 primeros datos del indicador de grupo:
head(ind)

# finalmente sólo armamos los dos Dataframes: para entrenar el modelo, y para probarlo con datos nuevos
training <- data[ind==1,]
testing <- data[ind==2,]

# Veamos los dos DataFrames
View(training)
View(testing)

################################
# 5. ENTRENAMIENTO DEL MODELO
# Recuerden: este es un problema supervisado de clasificación, nada más
# TOMAMOS gre, gpa, ranking Y CLASIFICAMOS EN admitido o no admitido
################################
# En este ejercicio vamos a usar un modelo llamado Red neuronal
# Para poder usarlo, tenemos que instalar el paquete estadístico que lo tiene implementado
install.packages("neuralnet")
# Y luego de instalarlo en el sistema, lo importamos a aquí para poder llamarlo
library(neuralnet)

# Guardamos los resultados de nuestro modelo en una nueva variable "n" en este caso
n <- neuralnet(admit~gre+gpa+rank,
               data = training,
               hidden = 2,
               err.fct = "ce",
               linear.output = FALSE)

################################################
# 6.1. PODEMOS VER RESULTADOS DEL APRENDIZAJE
################################################
# Con la función "plot" podemos ver la red entrenada!
plot(n)

# También podemos ver qué predice el modelo para los datos que ya conoce.
# Para ver esas predicciones, es diferente en cada modelo.
# Podemos buscar en internet para cada modelo cómo revisar las predicciones.
output <- compute(n, training[,-1])
head(output$net.result) # Este modelo nos devuelve la probabilidad de que la persona sea admitida
head(training[,]) # Estos son los datos de entrenamiento originales, para ver

################################
# 6.2. TAMBIÉN CON DATOS NUEVOS
################################
output_test <- compute(n, testing[,-1])
head(output_test$net.result) # La probabilidad de que la persona sea admitida
head(testing[,]) # Estos son los datos de entrenamiento originales, para ver


################################
# 7. EVALUACIÓN - REVISAMOS CUÁNTAS VECES HUBO ERROR EN LA PREDICCIÓN
################################

# Para evaluar la herramienta, empleamos una matriz que compara los valores predichos (filas)
# con los valores reales (en las columnas)
# El error de clasificación es el porcentaje de veces que la máquina se equivocó

# Matriz de confusión & Error de clasificación - datos conocidos (de entrenamiento)
output <- compute(n, training[,-1])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, training$admit)
tab1 # MATRIZ DE CONFUSIÓN
1-sum(diag(tab1))/sum(tab1)

# Ejercicio: Matriz de confusión & Error de clasificación - datos nuevos (de prueba)
output <- compute(n, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testing$admit)
tab2 # MATRIZ DE CONFUSIÓN
1-sum(diag(tab2))/sum(tab2)


# Ejercicio: correr el código nuevamente, pero cambiando la arquitectura de la red, 
# en lugar de una neurona en la capa oculta, ubiquemos dos capas ocultas, una con 2 neuronas,
# y otra con una. 
# Pista: para indicarlo, hay que proveer en alguna parte una columna c(2,1)
