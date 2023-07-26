
                                  #Trabajo Práctico "Caso de estudio modelo Linea
                                  #Estudiantes: Cebreiros Lucia y Mastelli Mateo

library(ggplot2)
library(GGally)
library(MASS)
library(boot)
library(leaps)
install.packages("leaps")
library(matlib)
install.packages("matlib")

# 1) Cargar los datos del archivo. La variable Following es una variable categórica donde el 1 indica si el 
#auto está siguiendo y 0 si no. Transformarla en un factor. Finalmente revisar que todas las variables
#contenidas en el dataframe estén correctamente definidas


  #Cargamos los datos
data<- read.csv("car-following_trajectory.csv")

  #transformamos la variable vs que es categorica
data$Following <- as.factor(data$Following)

  #Todo el resto de las variables estan correctamente definidas

# 2) Se desea ajustar un modelo de regresión múltiple para predecir la variable Local Y diff en función
#del resto de las variables en el data set. Escribir el modelo propuesto, indicando los supuestos del mismo

  #Variables

x1 = data$v_Vel
x2 = data$v_Acc
x3 = data$Space_Headway
x4 = data$Preceding_Distance
x5 = data$Following

betas = reg$coefficients
betas

  # Moledo de regresión lineal propuesto

y = betas[1] + x1 * betas [2] + x2 * betas [3] + x3 * betas [4] + x4 * betas [5] +
  x5 * betas [6]

  # Supuestos del modelo: 
    # Los errores ε_i tienen media cero. Lo cual implica E(ε)= 0
    # Los errores ε_i tienen todos la misma varianza, var(ε_i )=σ^2 (supuesto de homocedasticidad)
    # Los errores ε_i tienen distribución normal
    # Los errores ε_i son independientes entre sí y no están correlacionados con las covariables X_i
    
  # εi~N(0, σ2) independientes para cada i = 1, ... ,120

  # Se busca estimar es la esperanza de Y

#3)Realizar un scatterplot de las variables con la función ggpairs.

ggpairs(data)

  # Las variables V_vel, Preceding_Distance y Space_Headway guardan una fuerte correlación lineal
  #con la variable objetivo Local_Y_diff.
  # Las variables V_vel, Preceding_Distance y Space_Headway guardan un alto grado de linealidad entre sí.

#4) A partir de la tabla de correlaciones estimadas entre las variables, si tuviera que elegir una sola variable 
# para proponer un modelo de regresión #simple, ¿cuál eligiría y por qué?


  # Si solo se puede elegir una variable para proponer un modelo de regrsión simple, elerigíamos v_vel ya que 
  #tiene un mayor coeficiente de correlación siendo este 0,990. De todos modos, se ve que otras variables, 
  #como v_acc y predecing distance también tienen un alto grado de correlación lineal


#5) Realizar un ajuste de regresión lineal múltiple. ¿Es la regresión signiﬁcativa? Especiﬁcar las hipótesis 
#nula y alternativa de este test.¿Cómo se calcula el p-valor en este caso? ¿Rechazaría a un nivel de 
signiﬁcación de 0.05?.

  # Regresión lineal múltiple -> Se minimizan los residuos a través de mínimos cuadrados.

reg <-lm(Local_Y_diff~., data=data)

summary(reg)

  # Test de significación -> Comprobar que el modelo elegido representa mejor a la variable objetivo que 
  #considerar el promedio de la misma.
    #HIPÓTESIS: 
      # h0: todos lo beta son iguales e igual a cero 
      # h1: algún beta es distitno de cero
    # Buscamos rechazar h0 con un grado de confianza 1- alfa 
  # Se observa el p-valor -> probabilidad de que el estadístico del test sea más grande que el valor observado.
    
  # A un nivel de 0,05 queda rechazada la hipótesis h0. El valor límite para rechazar la hiótesis resultó 2,1e-16


#6) A partir de la tabla de coeﬁcientes estimados, ¿Qué variables resultan signiﬁcativas? ¿A qué nivel? 
#¿Cuál es el valor de la estimación para σ2? Especiﬁcar las hipótesis nulas y alternativas para alguno de los 
#test t reportados en la tabla, el estadístico del test y la regla de decisión.¿Cómo se calcula el p-valor para 
#este test?


confint(reg)

  #Las variables más significativas resultan (en orden de significación): 
    # V_vel con más del 2e-16, 
    #v_Acc con 9,1e-15, 
    #Preceding_Distance con 0,000942, 
    #Space_Headway con 0,008084
  # Estos valores resultan de un test de hipótesis para cada variable por separado, 
  # que nos informa su grado de significación.
  
  #El valor de sigma cuadrado (varianza) es 0,3242

#7)	Evaluar la bondad del ajuste realizado, a través del coeficiente de determinación. 
#Indicar cuánto vale y que significa.

  #R^2=0.9918 ;   R_a^2=0.9915 

  # R^2 - > mide cual es el porcentaje de la variabilidad de Y explicada por el modelo (medida de la 
  #capacidad de ajuste)
  # Si R^2 está cerca de 1 significa que el modelo propuesto aporta para explicar dicha variabilidad.
  # No sirve para comparar modelos que tengan diferente cantidad de variables (va aumentando su valor con 
  #la cantidad de covariables)
  # R2 ajustad -> Aumenta solamente si la variable que agregamos mejora el modelo.



#8)	Validar los supuestos expresados en el ítem 2 a partir del análisis de los residuos, 
#para el modelo seleccionado. ¿Observa algo extraño en los gráficos? ¿Qué propone?

  #Supuestos del modelo:
    # Los errores ε_i tienen media cero. Lo cual implica E(ε)= 0
    # Los errores ε_i tienen todos la misma varianza, var(ε_i )=σ^2 (supuesto de homocedasticidad)
    # Los errores ε_i tienen distribución normal
    # Los errores ε_i son independientes entre sí y no están correlacionados con las covariables X_i



ajustados<- reg$fitted.values
residuos<- reg$residuals
plot(ajustados, residuos, pch=20)
abline(h=0)

  # Los residuos siguen una leve forma cuadrática y hay un outlair hacia el final del gráfico
  # No debería observarse ninguna estructura, deberían aparecer puntos distribuidos aleatoriamente alrededor del cero
  # El hecho de que los residuos sigan una leve forma cuadrática es un indicativo de que al modelo planteado le falta
  #un término cuadrático

shapiro.test(residuios)

  #qqplot -> evaluar el supuesto de normalidad de los residuos 

qqnorm(residuos)
qqline(residuos)
  
  # Los puntos se despegan en los extremos por lo cual no se cumpliría el supuesto de normalidad.  
  # Este supuesto es el menos grave de no cumplir ya que si la muestra fuese infinita tendería a la normalidad


  #boxcox -> Para solucionar estos problemas se pueden aplicar las transformaciones Box y Cox

bc<- boxcox(Local_Y_diff~., data=data)
lamda<- bc$x[which.max(bc$y)]
lamda


  #Se realiza nuevamente la regresión lineal múltiple -> Nueva variable
y2<-(data$Local_Y_diff^lamda-1)/lamda

  # Nuevo data set con la variable objetivo reemplazada
data2<-data
data2$Local_Y_diff<-y2

reg2<-lm(Local_Y_diff~., data = data2)
summary(reg2)
residuos2<-reg2$residuals
qqnorm(residuos2)
qqline(residuos2)
points(residuos2,col="blue")
plot(reg2$fitted.values,residuos2$residuals,pch=20)
abline(h=0)

  #No se observa una mejora en el ajuste del modelo. Tampoco se observa una mejora con respecto a la normalidad.

#9)	¿Cuál seria la estimación de la esperanza de la variable a predecir para una observación con los 
#siguientes valores: v vel = 4,06, v Acc = 0,0568, Space Headway = 8,575, P receding Distance = 7,62, 
#F ollowing = 1

x0 = data.frame("v_Vel"=4.06, "v_Acc"=0.0568, "Space_Headway"=8.575, "Preceding_Distance"=7.62, "Following"= "1")
y0<- predict(reg, newdata=x0)
y0

  # y0 = 4.380859 m


#10)	Hallar un intervalo de confianza y de predicción de nivel 0.95 para la estimación hallada en el ítem anterior

int<- predict(reg, newdata=x0, interval = "confidence", level = 0.95)
int

intP<-predict(reg,, newdata=x0, interval = "prediction", level = 0.95)
intP

  # Esperanza de Y para el nuevo punto es 4.380859 m y se encuentra dentro del intervalo de confianza 
  #[4.051004 m; 4.710713] con un 95% de probabilidad.
  # Estimación de Y para el nuevo punto es 4.380859 m y se encuentra dentro del intervalo de
  #predicción [3.658903 m; 5.102815 m] con un 95% de probabilidad.


#11) Elección de modelos. Plantear un nuevo modelo en el que intervengan aquellas variables que contribuyen 
#significativamente y estimar los parámetros por mínimo cuadrados. ¿Qué modelo elegiría finalmente? Utilizar 
#medidas de bondad de ajuste y de predicción, tal como la estimación del error cuadrático medio de validación cruzada.

  # Utilizamos el método “Exhaustive”. Debido a que nuestro dataset no es muy grande, y que las variables del 
  #modelo son pocas. 
  # Otros métodos: forward y backward -> menos precisos pero computacionalmente más eficientes.

data3<-data[,c(1:4,6)]

reg3<-lm(Local_Y_diff~., data = data3)
summary(reg3)



attach(data)
x<-cbind(v_Vel,v_Acc,Space_Headway,Preceding_Distance,Following)

exhaustive<-regsubsets(Local_Y_diff~x,data = data, method = "exhaustive")
summary(exhaustive) 
names(summary(exhaustive))

par(mfrow=c(1,2))
plot(summary(exhaustive)$adjr2,pch=20,xlab="Modelo", ylab= "R^2 aj")
plot(1:5,summary(exhaustive)$cp,pch=20,ylim=c(0,8),xlab="Modelo", ylab= "CP")
abline(0,1)
par(mfrow=c(1,1))

  # El modelo que mejor ajusta es el que utiliza 4 variables ya que tiene el mayor coeficiente de
  # determinación y el cp de Mallows que más cerca está de la recta identidad.

  # Segundo modelo: Ra 2 = 0.9915 es igual que para el modelo de regresión con todas las variables y que, 
  #el modelo ganador resulta este último por contar con menos variables y ser menos complejo.


  # Medida de bondad de predicción -> estimación del ECM usando validación cruzada

P<- x%*%inv(t(x)%*%x)%*%t(x)
n = length(data$v_Vel)
r = reg$residuals

a = 0
for (i in n) {
a = a + r[i]^2/(1-P[i,i])
  }
ECM1 <- a/n
ECM1


r2 = reg2$residuals
a = 0
for (i in n) {
  a = a + r2[i]^2/(1-P[i,i])
}

ECM2 <- a/n
ECM2


  # El error cuadrático medio comprueba que el modelo 2 se ajusta mejor a la variable objetivo.


predichos<-c()
for(i in 1:nrow(datos.test))
{
  predichos[i]<-knn(train=datos.train$balance,cl=datos.train$default,
                    test=datos.test$balance[i],k=5)
}
L_sombrero<-mean(predichos != as.numeric(datos.test$default))








