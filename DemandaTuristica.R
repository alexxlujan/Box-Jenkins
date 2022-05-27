library(tseries)
library(outliers)
library(car)
library(forecast)
library(lmtest)
library(nortest)
library(flexmix)
library(TSA)
library(latex2exp)
library(astsa)

datos <- read.csv("CDMX-ACA.csv")
datos <- datos[305:424,]

# Formato de fecha
datos$fecha <- as.Date(datos$fecha_salida, "%d/%m/%Y")
pasajeros <- datos$pasajeros_transportados

#Grafica de los datos
plot(pasajeros_transportados ~ fecha, 
     datos, xaxt = "n", 
     type ="l", 
     xlab="Fecha", 
     ylab="Número de pasajeros", 
     main="Serie de tiempo del número de pasajeros")
axis(1, datos$fecha, format(datos$fecha, "%d-%m-%Y"), cex.axis = .7)

#Descomposicion de la serie de tiempo
descomposicion <- decompose(ts(datos$pasajeros_transportados, frequency = 7))
plot(descomposicion, xlab="observaciones",ylab="aleatoria estacional tendencia serie")

#box plot de la serie de tiempo
boxplot(datos$pasajeros_transportados, horizontal = TRUE,
        main="Boxplot de la serie de pasajeros transportados",
        xlab="Pasajeros")

#prueba para verificar outliers
grubbs.test(pasajeros, type = 10)


# Función para repetir el tratamiento de datos atípicos
outliers = TRUE
count_outliers = 0
while (outliers == TRUE) {
  test <- grubbs.test(pasajeros, type=10)
  if(test$p.value<0.05){
    pasajeros[pasajeros==max(pasajeros)] = 
      max(pasajeros[pasajeros!=max(pasajeros)])
    count_outliers=count_outliers+1
  } else{
    outliers = FALSE
  }
}
count_outliers

# Boxplot sin datos atípicos
boxplot(pasajeros, horizontal = TRUE)

plot(pasajeros ~ fecha, 
     datos, xaxt = "n", 
     type ="l", 
     xlab="Fecha", 
     ylab="Número de pasajeros", 
     main="Serie de pasajeros con tratamiento de outliers")
axis(1, datos$fecha, format(datos$fecha, "%d-%m-%Y"), cex.axis = .7)


#### ESTABILIZAR VARIANZA

grupo = c(rep("grupo1",length(pasajeros)/2),
          rep("grupo2",length(pasajeros)/2))

leveneTest(pasajeros, grupo)
lambda <- BoxCox.lambda(pasajeros)  # 1
#UTILIZANDO GUERRERO TENEMOS
lambda <- -0.15
pasajeros <- pasajeros^lambda

#Grafica de la serie con la varianza estable
plot(pasajeros ~ fecha, 
     datos, xaxt = "n", 
     type ="l", 
     xlab="Fecha", 
     ylab="Número de pasajeros (escala ajustada)", 
     main=TeX("Serie de pasajeros con transformación de potencia ($\\lambda$=-0.15)"))
axis(1, datos$fecha, format(datos$fecha, "%d-%m-%Y"), cex.axis = .7)

leveneTest(pasajeros, grupo)


##### ESTABILIZAR TENDENCIA
# Prueba de Dickey Fuller para verificar tendencia
adf.test(pasajeros)

pasajeros_diff <- diff(pasajeros)
plot(pasajeros_diff, type='l', main="Serie de pasajeros estacionaria",
     xlab="observación", ylab="valor")

adf.test(pasajeros_diff) #Ya no tiene tendencia

acf(pasajeros_diff, lag.max = 60, main="ACF de la serie de pasajeros estacionaria")
pacf(pasajeros_diff, lag.max = 60, main="PACF de la serie de pasajeros estacionaria")

pasajeros_diff <- diff(pasajeros_diff, 7) #Diferencia estacional
plot(pasajeros_diff, type='l', main="Serie de pasajeros con una diferencia de orden 7",
     xlab="observación", ylab="valor")

grubbs.test(pasajeros_diff, type=10)

grupo = c(rep("grupo1",(length(pasajeros_diff))/2),
          rep("grupo2",length(pasajeros_diff)/2))
leveneTest(pasajeros_diff, grupo)

acf(pasajeros_diff, lag.max = 60, main="ACF de la serie de pasajeros con diferencia estacional")
pacf(pasajeros_diff, lag.max = 60, main="PACF de la serie de pasajeros con diferencia estacional")


## Modelos propuestos
model1 <- arima(pasajeros, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 7))
model2 <- arima(pasajeros, order = c(0,1,1), seasonal = list(order = c(2,1,0), period = 7))
model3 <- arima(pasajeros, order = c(2,1,0), seasonal = list(order = c(2,1,0), period = 7))


# Admisibilidad del modelo
coeftest(model1) 
coeftest(model2) 
coeftest(model3) 

model1$aic
BIC(model1)

model2$aic
BIC(model2)

model3$aic
BIC(model3)

cat("\n  modelo1  |  modelo2  |  modelo3", "\n",
    model1$aic,"|",model2$aic,"|",model3$aic,"| aic", "\n",
    BIC(model1),"|",BIC(model2),"|",BIC(model3),"| bic")

# supuestos de los residuos

# Grafica de los residuos
plot(model3$residuals)

##media cero
t.test(model3$residuals)

##varianza constante
grupo = c(rep("grupo1",length(pasajeros)/2),
          rep("grupo2",length(pasajeros)/2))

leveneTest(model3$residuals, grupo)

# No correlacion
acf(model3$residuals, main="ACF de los residuos del modelo 3")
pacf(model3$residuals, main="PACF de los residuos del modelo 3")

# Normalidad
lillie.test(model3$residuals)


# No existencia de datos atípicos
boxplot(model3$residuals, horizontal = TRUE,
        main="Boxplot de los residuos del modelo3",
        xlab="Pasajeros")

grubbs.test(model3$residuals, type = 10)

#Pronósticos
pasajeros <-datos$pasajeros_transportados
pronostico = sarima.for(pasajeros, n.ahead=14, #Pronósticamos dos semanas.
                        p=2,d=1,q=0,P=2,D=1,Q=0,S=7)
