#########EJERCICIO UNO##########

#Afijación óptima
Nh<-c(7,25) 
sh<-c(1318,331) #desv est. de A y B
ch<-c(10,25) #costos de A y B
N<-sum(Nh)
N
Wh<-Nh/N
Wh
n<-8

nh<-n*(Nh*sh/sqrt(ch))/(sum((Nh*sh)/sqrt(ch)))
round(nh,0)

#Afijación Proporcional
nh2<-Wh*n
nh2<-round(nh2,0)
nh2

#Afijación de Neyman
nh3<-(n*Nh*sh/(sum(Nh*sh)))
nh3<-round(nh3,0)
nh3
sum(nh3)


##################################################################################################################
##################################################################################################################
###pregunta dos###

y<-c(50,53,44,45,53,31,35,45,34,44,52,52)
x<-c(1005,1072,884,907,1068,625,705,909,692,891,1046,1052)


##a) Calcule la correlación entre x e y2/x
correla<-cor(x, (y^2)/x) #cor sirve para analizar correlaciones tipo matriz
correla


##b) Realice un gráfico de dispersión para y/x y explique si se puede afirmar que la razón
    #es constante para los elementos de la población.
par(mfrow=c(1,2)) # Se crea una matriz de gráficos 1 x 2, esto es solo para el tamaño del gráfico
plot(y/x)
abline(h = mean(y/x))

#Este es un gráfico matricial, podemos observar que la razón no es constante para los elementos en esta población



#c) Utilice el análisis de regresión simple para estimar el valor del intercepto y decida si
    #este es estadísticamente diferente de cero.
modelo<-lm(y~x)
summary(modelo)




##################################################################################################################
##################################################################################################################
#########################SOLUCIÓN CASO############################
library(TeachingSampling)
pc2=read.delim("clipboard")
pc2
######################
#1.
str(pc2)
N<-dim(pc2)[1]
names(pc2)
attach(pc2)
m<-12
pc2$TALLA..m.<-as.numeric(pc2$TALLA..m.)
pk<-pc2$TALLA..m./sum(pc2$TALLA..m.)
RNGkind(sample.kind = "Rounding")
set.seed(20)
sam<-S.PPS(m,pc2$TALLA..m.)
sam
muestra2<-pc2[sam,]
muestra2
nrow(muestra2)
head(muestra2)

# 2.Estime el Perímetro Media de Brazo( PMB) , error estándar y coeficiente de variación.
pk.s<-pk[sam]
attach(muestra2)
estimación=E.PPS(muestra2$PMB..cm.,pk.s)/N
estimación

# 3. Halle e interprete el intervalo de confianza la 95 % para el Perímetro Media de Brazo( PMB)
LI<-es[1,2]-qnorm(1-0.05/2)*es[2,2]
LS<-es[1,2]+qnorm(1-0.05/2)*es[2,2]
c(LI,LS)

# 4.Verifique si la estrategia de muestreo seleccionado cumple con las condiciones de
  #optimalidad. Use el efecto diseño

par(mfrow=c(1,2))
plot(sam/)
abline(h=mean(sam/),col=2)
plot(sam/)
abline(h=mean(sam/),col=2)
