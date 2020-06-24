#Función para el calculo de rendimientos de cada uno de los activos en cierto dataframe
log_return_calc=function (log_dataframe){
  return((diff(log(log_dataframe))))
}

#Creación de histogramas
hist_creation=function(dataFram_log_returns){
  counter=1
  datframe_temp=array(0, dim = c(dataFram_log_returns))
  for( i in colnames(dataFram_log_returns)){
    breaks <- pretty(range(dataFram_log_returns[[i]]), n = nclass.FD(dataFram_log_returns[[i]]), min.n = 1)
    bwidth <- breaks[2]-breaks[1]
    ggplot(dataFram_log_returns,mapping=aes(x=dataFram_log_returns[[i]]))+ 
             geom_histogram(binwidth=bwidth,fill="steelblue",colour="black")+
             xlab(paste(i,"_Returns"))
  }
}

getTStime <- function(ats){
  start <- start(ats)
  end <- end(ats)
  time <- list()
  time[[1]] <- start
  m <- 2
  while(!(identical(start, end))){
    start[2] <- start[2] + 1
    if (start[2]==13){
      start[1] <- start[1] + 1
      start[2] <- 1
    }
    time[[m]] <- start
    m <- m + 1
  }
  return(time)
}


#Creación de ChartSeries
chart_creation=function(dataFram_log_returns){
  for( i in colnames(dataFram_log_returns)){
    print(i)
    jpeg(paste("img_plots/chart_series_",i,".jpg"))
    chartSeries(dataFram_log_returns[i],type="lines", name = i, show.grid = FALSE, up.col = "steelblue")
    dev.off()
  }
}


#calculo de Betas en relación al Mercado
beta_calculation=function(volatility,benchmark_volatility,correlation_matrix_benchmark,colnames_vec){
  betas <- (volatility/benchmark_volatility)*correlation_matrix_benchmark
  colnames(betas) <- colnames_vec
  return(betas)
  
}


#sharpeRatio Calculation

sharpeRatio_calculation=function(return_port, risk_free,sd_port_cre){
  return((return_port-risk_free)/sd_port_cre)
}

#Ration de Treynor
treynor_calculation=function(return_port,risk_free,weights_port,betas_port){
  multiplication_matrix=weights_port%*%betas_port
  return((return_port-risk_free)/(multiplication_matrix[[1]]))
}

#calculo de Alpha de Jensen
Jensen_alpha_calculation=function(return_port,risk_free,weights_port,betas_port,mean_return_benchmark){
  multiplication_matrix=weights_port%*%betas_port
  return((return_port-risk_free)-((multiplication_matrix)*mean_return_benchmark-risk_free))
}

#trecking Error
trecking_error_calculation=function(return_port,mean_return_benchmark){
  return(sd(return_port-mean_return_benchmark))
  
}

#information ratio

information_ratio_calculation=function(trecking_error,jesen_alpha){
  return(jesen_alpha/trecking_error)
}

#Pruebas de Normalidad de Todos los activos
normalTests=function(dataFrame_Test_norm){
  for( i in colnames(dataFrame_Test_norm)){
    print(i)
    writeLines(capture.output(print(jarqueberaTest(dataFrame_Test_norm[[i]]))), con = file(paste("pruebas_normalidad/normalidad_",i,".txt")))
  }
}

#Método de Impresión de Resultados
printing_results=function(info_print,name_file){
    writeLines(capture.output(print(info_print)), con = file(paste(name_file,".txt")))
}
#Calculo de Media de cada Activo
meanCalculation=function(dataFrameCalculate_Mean){
  return(dataFrameCalculate_Mean%>%summarise_all(mean))
}

#Portafolio de pesos iguales
getPortfolio <-function(er, cov.mat, weights){
    call <- match.call()
    asset.names <- names(er)
    weights <- as.vector(weights)
    names(weights) = names(er)
    er <- as.vector(er)                 # assign names if none exist
    if(length(er) != length(weights))
      stop("dimensions of er and weights do not match")
    cov.mat <- as.matrix(cov.mat)
    if(length(er) != nrow(cov.mat))
      stop("dimensions of er and cov.mat do not match")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    er.port <- t(er)%*%weights
    sd.port <- sqrt(weights %*% cov.mat %*% weights)
    return(list("call" = call,
                "er" = as.vector(er.port),
                "sd" = as.vector(sd.port),
                "weights" = weights) )
  }

#Portafolio de minima varianza
globalMin.portfolio <- function(er, cov.mat, shorts=TRUE){
  call <- match.call()
  asset.names <- names(er)
  er <- as.vector(er) # assign names if none exist
  cov.mat <- as.matrix(cov.mat)
  N <- length(er)
  if(N != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  if(shorts==TRUE){
    cov.mat.inv <- solve(cov.mat)
    one.vec <- rep(1,N)
    w.gmin <- rowSums(cov.mat.inv) / sum(cov.mat.inv)
    w.gmin <- as.vector(w.gmin)
  } else if(shorts==FALSE){
    Dmat <- 2*cov.mat
    dvec <- rep.int(0, N)
    Amat <- cbind(rep(1,N), diag(1,N))
    bvec <- c(1, rep(0,N))
    result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
    w.gmin <- round(result$solution, 6)
  } else {
    stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")}
  names(w.gmin) <- asset.names
  er.gmin <- crossprod(w.gmin,er)
  sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
  return(list("call" = call,"er" = as.vector(er.gmin),"sd" = as.vector(sd.gmin),"weights" = w.gmin))
}

#Portafolio Eficiente
efficient.portfolio <-function(er, cov.mat, target.return, shorts=TRUE)  {
    call <- match.call()
    asset.names <- names(er)
    er <- as.vector(er) # assign names if none exist
    N <- length(er)
    cov.mat <- as.matrix(cov.mat)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    if(shorts==TRUE){
      ones <- rep(1, N)
      top <- cbind(2*cov.mat, er, ones)
      bot <- cbind(rbind(er, ones), matrix(0,2,2))
      A <- rbind(top, bot)
      b.target <- as.matrix(c(rep(0, N), target.return, 1))
      x <- solve(A, b.target)
      w <- x[1:N]
    } else if(shorts==FALSE){
      Dmat <- 2*cov.mat
      dvec <- rep.int(0, N)
      Amat <- cbind(rep(1,N), er, diag(1,N))
      bvec <- c(1, target.return, rep(0,N))
      result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=2)
      w <- round(result$solution, 6)
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    names(w) <- asset.names
    er.port <- crossprod(er,w)
    sd.port <- sqrt(w %*% cov.mat %*% w)
    return(list("call" = call,
                "er" = as.vector(er.port),
                "sd" = as.vector(sd.port),
                "weights" = w) )
  }


#Portafolio de Tangencia
tangency.portfolio <-function(er,cov.mat,risk.free, shorts=TRUE){
  call <- match.call()
  asset.names <- names(er)
  #if(risk.free < 0)
   # stop("Risk-free rate must be positive")
  er <- as.vector(er)
  cov.mat <- as.matrix(cov.mat)
  N <- length(er)
  if(N != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  gmin.port <- globalMin.portfolio(er, cov.mat, shorts=shorts)
  if(gmin.port$er < risk.free)
    stop("Risk-free rate greater than avg return on global minimum variance portfolio")
  if(shorts==TRUE){
    cov.mat.inv <- solve(cov.mat)
    w.t <- cov.mat.inv %*% (er - risk.free) # tangency portfolio
    w.t <- as.vector(w.t/sum(w.t))          # normalize weights
  } else if(shorts==FALSE){
    Dmat <- 2*cov.mat
    dvec <- rep.int(0, N)
    er.excess <- er - risk.free
    Amat <- cbind(er.excess, diag(1,N))
    bvec <- c(1, rep(0,N))
    result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
    w.t <- round(result$solution/sum(result$solution), 6)
  } else {
    stop("Shorts needs to be logical. For no-shorts, shorts=FALSE.")
  }
  names(w.t) <- asset.names
  er.t <- crossprod(w.t,er)
  sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
  return(list("call" = call,
                   "er" = as.vector(er.t),
                   "sd" = as.vector(sd.t),
                   "weights" = w.t))
}


#Frontera Eficiente
efficient.frontier <- function(er, cov.mat, nport=100, alpha.min=-0.5, alpha.max=1.5, shorts=TRUE){
  call <- match.call()
  asset.names <- names(er)
  er <- as.vector(er)
  N <- length(er)
  cov.mat <- as.matrix(cov.mat)
  if(N != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  port.names <- rep("port",nport)
  ns <- seq(1,nport)
  port.names <- paste(port.names,ns)
  
  cov.mat.inv <- solve(cov.mat)
  one.vec <- rep(1, N)
  port.gmin <- globalMin.portfolio(er, cov.mat, shorts)
  w.gmin <- port.gmin$weights
  
  if(shorts==TRUE){
    er.max <- max(er)
    port.max <- efficient.portfolio(er,cov.mat,er.max)
    w.max <- port.max$weights    
    a <- seq(from=alpha.min,to=alpha.max,length=nport) # convex combinations
    
    we.mat <- a %o% w.gmin + (1-a) %o% w.max           # rows are efficient portfolios

    er.e <- we.mat %*% er                                          # expected returns of efficient portfolios
    er.e <- as.vector(er.e)
  } else if(shorts==FALSE){
    we.mat <- matrix(0, nrow=nport, ncol=N)
    we.mat[1,] <- w.gmin
    we.mat[nport, which.max(er)] <- 1
    er.e <- as.vector(seq(from=port.gmin$er, to=max(er), length=nport))
    for(i in 2:(nport-1)) 
      we.mat[i,] <- efficient.portfolio(er, cov.mat, er.e[i], shorts)$weights
  } else {
    stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
  }
  
  names(er.e) <- port.names
  
  cov.e <- we.mat %*% cov.mat %*% t(we.mat) # cov mat of efficient portfolios
  sd.e <- sqrt(diag(cov.e))                           # std devs of efficient portfolios
  sd.e <- as.vector(sd.e)
  names(sd.e) <- port.names
  dimnames(we.mat) <- list(port.names,asset.names)
  
  return(list("call" = call,
              "er" = er.e,
              "sd" = sd.e,
              "weights" = we.mat))
}

#Markovitz
plot.Markowitz <- function(object, plot.assets=FALSE,name_file,color_paint,port_min,port_equ,port_tan,port_obj,risk_free, ...){
    if (!plot.assets) {
      y.lim=c(0,max(object$er))
      x.lim=c(0,max(object$sd))
      plot(object$sd,object$er,type="b",xlim=x.lim, ylim=y.lim,
           xlab="Portfolio SD", ylab="Portfolio ER", 
           main="Efficient Frontier", ...)
    }else {
      call = object$call
      mu.vals = eval(call$er)
      sd.vals = sqrt( diag( eval(call$cov.mat) ) )
      y.lim = range(c(0,mu.vals,object$er))
      x.lim = range(c(0,sd.vals,object$sd))
      

      plot(object$sd,object$er,type="b", xlim=x.lim, ylim=y.lim,
           xlab="Portfolio SD", ylab="Portfolio ER", 
           main=name_file, ...)
      text(sd.vals, mu.vals, labels=names(mu.vals))


    }
    invisible()
    #Agregado de Funcsión
    
    points(port_min$sd, port_min$er, col=color_paint[1],pch=15)
    port_equ$er=mean(port_equ$er)
    points(port_equ$sd,port_equ$er,col=color_paint[2],pch=15)
    
    points(port_tan$sd, port_tan$er, col=color_paint[3],pch=15)
    
    points(port_obj$sd,port_obj$er,col=color_paint[4],pch=15)
    
    sr.tan = (port_tan$er - risk_free)/port_tan$sd
    abline(a=risk_free, b=sr.tan)    
}


#Plot Efficient Frontier
plot_efficient_frontier=function(dataPlot,name_file){
  return(plot(dataPlot$er*100~dataPlot$sd,main=name_file,ylab='Rentabilitat (en %)', xlab='Risc',col='red')   )
}


#Plots de Barras de comparación
plot_bar_diff=function(dataObjectPlot,name_plot_bar,color_paint){
  counter=1
  for( i in dataObjectPlot){
  barplot(i$weights, names=names(i$weights),
          xlab="Assets", ylab="Weight", main=name_plot_bar[counter],col=color_paint[counter])
  counter=counter+1
  }
}

#función de Cración de Barplot singleton
create_single_barplot=function(name_plot_bar,weights_port,columns_names,color_plot){
  
  barplot(weights_port, xlab="Assets",names=columns_names, ylab="Weight", main='Weights Selected Portfolio',col=color_plot, las=2, cex.names=0.8)

}

#Gneración de Volatilidades
volatilities_calculationa_assets=function(dataFrameCalculate_sd){
  return(dataFrameCalculate_sd%>%summarise_all(sd))
}


#Normal Var
normal_var_generation=function(generation_var,weight_portfolio){
  data_frame_temp=data.frame(matrix(0,length(weight_portfolio),25))
  colnames(data_frame_temp)=99:75
  counter=99
  for (t in generation_var){
    data_frame_temp[toString(counter)]=t*weight_portfolio
    counter=counter-1
  }
  return(data_frame_temp)
}

#Generador de Volatilidades
volatilities_generation=function(asset_volatilities){
  normal_dist_volat=data.frame(matrix(0,length(asset_volatilities),25))
  colnames(normal_dist_volat)=99:75
  counter=1
  for (j in asset_volatilities){
      for (i in 99:75){
        normal_dist_volat[counter,100-i]=j*(qnorm((i/100), mean=0, sd=1, lower.tail = T))
      }
    counter=counter+1
    }
  
  return(normal_dist_volat)
}


#Var No diversificado
var_none_diversified=function(array_VAR,faceValue){
  array_temp=0
  for(k in array_VAR){
    array_temp=array_temp+sum(array_VAR[k])
  }
  return((array_temp/faceValue)*100)
}


var_corr_Nonediversified=function(array_VAR,faceValue,correlarion_matrix){
  array_temp=data.frame(matrix(0,1,length(c(99:75))))
  colnames(array_temp)=colnames(array_VAR)
  for(k in colnames(array_VAR)){
    array_temp[k]= sqrt(t(array_VAR[[k]]) %*% correlarion_matrix %*% array_VAR[[k]])
  }
  percentage_change=(array_temp/faceValue)*100
  
  return(percentage_change)
  
  
}



cvar_corr_diversified=function(returns_port,array_VAR,port_selected_weights,functionUse){
  array_temp=data.frame(matrix(0,1,length(c(99:75))))
  colnames(array_temp)=colnames(array_VAR)
  CVAR_95_assets=0
  CVAR_calcl=0
  counter=0.99
  for(k in colnames(array_VAR)){
    CVAR_95_assets=functionUse(returns_port,p=counter)
    CVAR_calcl=port_selected_weights%*%t(CVAR_95_assets)
    array_temp[k]=CVAR_calcl[[1]]
    counter=counter-0.01
  }
  new_values=array_temp
  return(array_temp)
  
  
}



var_diversified=function(returns_port,array_VAR,functionUse,port_selected_weights){
  array_temp=data.frame(matrix(0,1,length(c(99:75))))
  colnames(array_temp)=colnames(array_VAR)
  CVAR_95_assets=0
  CVAR_calcl=0
  counter=0.99
  for(k in colnames(array_VAR)){
    CVAR_95_assets=functionUse(returns_port,p=counter)
    CVAR_calcl=port_selected_weights%*%t(CVAR_95_assets)
    array_temp[k]=abs(CVAR_calcl[[1]])
    counter=counter-0.01
  }
  new_values=array_temp
  return(array_temp)
  
  
}