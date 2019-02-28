

library(ggplot2)
library(Hmisc)
library(lattice)
library(latticeExtra)
library(QuantPsyc)

#para leer archivos  delimitados en texto
Album.Sales=read.delim(file.choose())
View(Album.Sales)
str(Album.Sales)
#Colocar de qu se trata cada variable
summary(Album.Sales)
#Si tiene missin values tener cuidado tiene que quitarlos porque no ejecuta comandos de regresión
albumSalesRegression=lm(sales~adverts+airplay+attract,data=Album.Sales)
summary(albumSalesRegression)
plot(Album.Sales$sales,Album.Sales$adverts)
#*** los niveles de estrellas me dicen el nivel de significancia del modelo
lm.beta(albumSalesRegression)
#para estandarizar los betas y saber a 1 desviación estandar de incremento 51% de la desviación estandar de x
sd(Album.Sales$adverts)
sd(Album.Sales$sales)
#intervalos de confianza de los betas
confint(albumSalesRegression)

#****************************************************************
#BLUE
#Meli->Mejor Estimador Lineal Insesgado

cor(as.matrix(Album.Sales))
#Multicolinealida
#VIF(Variance Inflation Factor) para medir multicolinealidad
vif(albumSalesRegression)
#Que el numero no sea mayor a 10... porque habria multicolinealidad
mean(vif(albumSalesRegression))
#El promedio que no sea muy alto de 1


#Heterosedasticidad
#El estimador ya no es el de minima varianza por heterocedasticidad
#hacer el modelo tomando en cuenta que es heterocedastico
install.packages("robustbase")
library(robustbase)
modelo_robusto=lmrob(sales~adverts+airplay+attract,data=Album.Sales)
#detectar heterocedasticidad
#prueba Breusch Pragan
install.packages("lmtest")
library(lmtest)
bptest(albumSalesRegression)
summary(modelo_robusto)




#Autocorrelación
#Durbin Watson
durbinWatsonTest(albumSalesRegression)
# que el D-W Statistic este entre 1 y  3 por los niveles de autocorrelación
#Si hay autocorrelación hay heterocedasticidad


#Normalidad en los residuales
plot(albumSalesRegression)
#El primer enter es para ver si heterocedastico
#segundo enter es para ver si esta parado sobre la normal la linea recta es la normal
#tercer click Los residuales estandarizados
Album.Sales$resid=residuals(albumSalesRegression)
Album.Sales$fitted=fitted(albumSalesRegression)
#si hay relación entre el error y  X hay un problema de variable omitida
install.packages("normtest")
library(normtest)
ajb.norm.test(Album.Sales$resid)


#para publicar
install.packages("stargazer")
library(stargazer)
stargazer(albumSalesRegression,type="html",out="Model.doc")


#para colocar todo en el mismo panel
install.packages("texreg")
library(texreg)
screenreg(list(albumSalesRegression,modelo_robusto))










