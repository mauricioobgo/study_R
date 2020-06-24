##*******************************************************
##Autor:Mauricio Obando Gómez
##Descripción: Análisis para descripción de indices y
##escogencia de indices con menor volatilidades y menor VAR
##*******************************************************
library(quantmod)
library(PortfolioAnalytics)
library(fBasics)
#Script de funciones General
setwd("~/Mauricito_carpeta/Gestión_Portafolios/PROYECTO_FINAL/script_r")
source("functions_port_mg.R")
library(TTR)
#Paquete de manejo de dataframes
library(dplyr)
#paquete de manejo de time series
library(imputeTS)
library(ggplot2)

library(dplyr)
library(tidyverse)
library(summarytools)
library(knitr)
library(nortest)


##### Construcción de Datos ######
#Aqui van los ticker de las compañías requeridas
#simbolos de Generación  de precios
nominal_investment=10000000
#symbols_vector=c( "HBIO" , "AAON" ,  "VBND" , "FOSL"  , "ADBE" , "COKE" , "CSCO" ,"CXDC" , "CATC" , "GOOGL" , "EBAY" , "MDLZ", "SNY","RMCF","MELI","PEP","MA","ABBV" ,"AXP","WFC")
symbols_vector=c("FB","AMD","CSCO","AON","AMT","PFE","MRK","XOM","LRCX","NVDA","ADBE","ORCL","T","JPM","MMC","BLK","USB","C","SBUX","VZ","USO","TLT")
#TLT ETF Treasuries 20 years
#United States Oil Fund, LP USO
(getSymbols(symbols_vector, from ="2015-01-01", to = "2020-05-15",periodicity = "weekly"))
#cambio de variables que tienen nombres relativos 
#atandt=`T`
#City=`C`
dataFrameGeneral=list(FB,AMD,CSCO,AON,AMT,PFE,MRK,XOM,LRCX,NVDA,ADBE,ORCL,atandt,JPM,MMC,BLK,USB,City,SBUX,VZ,TLT,USO)
dataFrameGeneral=data.frame(dataFrameGeneral)
dataFrameGeneral=na.omit(dataFrameGeneral)
#Eliminación de Variables
rm(list =symbols_vector)
#Selección de Precios con bajo la misma característica
dataFrameGeneral_prices_Close=dataFrameGeneral %>% select(contains("Close"))
#Eliminación de DataFrame General por performance en procesamiento posterior
rm(dataFrameGeneral)

### Comienzo de construcción de Portafolio #######
#calculo de retorno ln del portafolio

port_returns=data.frame(apply(dataFrameGeneral_prices_Close,2,log_return_calc))
#Generación de imágenes de histogramas
hist_creation(port_returns)
#Generación de ChartSeries
chart_creation(dataFrameGeneral_prices_Close)
#Generación de summary de todas las variables
view(dfSummary(port_returns))
#Descipción de Variables General
descr(port_returns)


### Generación de Matriz de COv ######

  ##Pruebas de Normalidad
normalTests(port_returns)

  ## Matriz de Varianzas y covarianzas ##

#MAtriz de covarianzas
covariance_matrix_pearson=cov(port_returns, y = NULL, use = "everything",
                               method = c("pearson"))
covariance_matrix_kendall=cov(port_returns, y = NULL, use = "everything",
                               method = c("kendall"))
covariance_matrix_Spearman=cov(port_returns, y = NULL, use = "everything",
                                method = c("spearman"))

#Matriz de correlaciones para examinar relación entre los activos
correlation_matrix_pearson=cor(port_returns, y = NULL, use = "everything",
    method = c("pearson"))
correlation_matrix_kendall=cor(port_returns, y = NULL, use = "everything",
                       method = c("kendall"))
correlation_matrix_Spearman=cor(port_returns, y = NULL, use = "everything",
                                method = c("spearman"))


### Generación de los distintos Portafolios ####
# Pesos de distribución igual para dar comienzo al proceso
equal_distr_weights=rep(1,length(symbols_vector))/length(symbols_vector)

#Portafolio de Pesos iguales inicialización variables
# se toma matriz de varianzas y cov de Spearman
#calculo de las medias de los retornos
mean_returns=meanCalculation(port_returns)
#Calculo de Portafolio dist. uniforme
port_equaly_Gen=getPortfolio(mean_returns,covariance_matrix_Spearman,equal_distr_weights)
printing_results(port_equaly_Gen,"portafolio_uniforme")

#Portafolio de Minima Varianza
#Con Cortos

#Se utiliza la transpuesta de los retornos medios por como esta contruida las funciones
port_min_variance_cortos=globalMin.portfolio(t(mean_returns),covariance_matrix_Spearman)
printing_results(port_min_variance_cortos,"portafolio_min_var_cortos")
#Sin Cortos
port_min_variance=globalMin.portfolio(t(mean_returns),covariance_matrix_Spearman,F)
printing_results(port_min_variance,"portafolio_min_var")


#Portafolio Objetivo
#Portafolio de Retorno del BenchMark en este caso NASDAQ
mean_return_bench_mark=0.000532641922342418
#Portafolio objetivo con cortos
portafolio_objetivo_cortos=efficient.portfolio(t(mean_returns),covariance_matrix_Spearman,mean_return_bench_mark)
printing_results(portafolio_objetivo_cortos,"portafolio_objetivo_cortos")
#Portafolio Objetivo sin cortos
portafolio_objetivo=efficient.portfolio(t(mean_returns),covariance_matrix_Spearman,mean_return_bench_mark,F)
printing_results(portafolio_objetivo,"portafolio_objetivo")


#Portafolio de Tangencia
#Retorno de Trasury a 10 años
retorno_libr_riesgo=-0.00372987649712883
portafolio_tangencia_cortos=tangency.portfolio(t(mean_returns),covariance_matrix_Spearman,retorno_libr_riesgo)
printing_results(portafolio_tangencia_cortos,"portafolio_tangencia_cortos")

#Portafolio de Tangecia sin COrtos
portafolio_tangencia=tangency.portfolio(t(mean_returns),covariance_matrix_Spearman,retorno_libr_riesgo,F)
printing_results(portafolio_tangencia,"portafolio_tangencia")

#Frontera Eficiente

frontera_eficiente_cortos=efficient.frontier(t(mean_returns),covariance_matrix_Spearman)
plot_efficient_frontier(frontera_eficiente_cortos,"frontera_eficiente_cortos")
#sin cortos
frontera_eficiente=efficient.frontier(t(mean_returns),covariance_matrix_Spearman,shorts=F)
plot_efficient_frontier(frontera_eficiente,"frontera_eficiente")



#### Comparación de Portafolios #####

#Colores deseados de los plots
color_use_plots=c("blue","green","orange","yellow")
#Nombre en orden de los plots

array_General_models_cortos=list(port_equaly_Gen,port_min_variance_cortos,portafolio_objetivo_cortos,portafolio_tangencia_cortos)
array_General_models=list(port_equaly_Gen,port_min_variance,portafolio_objetivo,portafolio_tangencia)

names_plots_cortos=c("uniform_portfolio_cortos","min_variance_portfolio_cortos","objective_portfolio_cortos","tangent_portfolio_cortos")
names_plots=c("uniform_portfolio","min_variance_portfolio","objective_portfolio","tangent_portfolio")

#plots_Cortos
plot_bar_diff(array_General_models_cortos,names_plots_cortos,color_use_plots)
plot_bar_diff(array_General_models,names_plots,color_use_plots)
#Se imporime plot de markovitz en la carpeta de trabajo con el nombre de MArkovitz Plot
plot.Markowitz(frontera_eficiente_cortos,plot.assets = T,"Markovitz_Plot_cortos",color_use_plots,port_min_variance_cortos,port_equaly_Gen,portafolio_tangencia_cortos,portafolio_objetivo_cortos,retorno_libr_riesgo)
plot.Markowitz(frontera_eficiente,plot.assets = T,"Markovitz_Plot",color_use_plots,port_min_variance,port_equaly_Gen,portafolio_tangencia,portafolio_objetivo,retorno_libr_riesgo)

#### Selección de Portafolio Eficiente ####

port_selected_returns=sort(frontera_eficiente$er*100,decreasing = T)
port_selected_standard=sort(frontera_eficiente$sd,decreasing = T)
port_selected_weights=frontera_eficiente$weights[2,]

# Numero aleatorio de Portafolio Eficiente
#imprimiendo Portafolio Seleccionado

create_single_barplot("Porfolio_Selected",port_selected_weights,symbols_vector,"purple")
printing_results(paste("Retorno_Esperado :",port_selected_returns[2],"  ","Estandard Dev :",port_selected_standard[2] ,"weights : ", port_selected_weights),"Portafolio_Seleccionado_Sin_Cortos")

# variable mean_returns, sd Returns
#desviaciones Anuales
sd_returns= volatilities_calculationa_assets(port_returns)*sqrt(52)

#Método Genera Volatilidades Hasta del 75% de confianza
volatilidades_assets_port=volatilities_generation(sd_returns)
percentage_change_volatilidades=normal_var_generation(volatilidades_assets_port,port_selected_weights)
ValueAtVar=percentage_change_volatilidades*nominal_investment

#Valor en %
variables_comparison=c("99","97","95","90")
none_diversified_var=var_none_diversified(variables_comparison,ValueAtVar,nominal_investment)
printing_results(ValueAtVar,"none_diversified_var")
none_diversified_corr=var_corr_Nonediversified(variables_comparison,ValueAtVar,nominal_investment,correlation_matrix_Spearman)
printing_results(none_diversified_corr,"none_diversified_corr")
