#remember to use checkpoint on (y)our local R installation!
#source('S:/AM_PS/rlib/libraryLoader.R')
#libraryLoader('2020-02-28', TRUE)
#library("matrixcalc")
#library("dplyr")
#library("vars")
#library("rmgarch")

#########################
########## AR ###########
#########################
AR <- function(y, lag = 1){
  rho <- matrix(pacf(y,lag, plot = F)[["acf"]], lag, 1)
  delta <- mean(y)/(1-sum(rho)); sigma <- sqrt(var(y)/(1-sum(rho)^2));
  par <- c(delta, rho, sigma)
  
  ly <- matrix(NA, length(y), lag)
  for (i in 1:lag){
    ly[,i] <- lag(y,i)
  }
  
  mle <- function(par){
    delta <- par[1]; rho <- matrix(par[2:(length(par)-1)],lag,1); sigma <- par[length(par)]
    meaneq <- y-ly%*%rho
    f <- dnorm(meaneq, delta, sigma)
    lf <- sum(log(f[(lag+1):length(f)]))
    return(-lf)
  }
  
  est <- optim(par,mle, hessian = TRUE); par <- as.data.frame(est[["par"]]);names(par) <- c("Estimates")
  fisher <- solve(est$hessian); err <- c(sqrt(diag(fisher)));
  sigma <- par$Estimates[length(par$Estimates)]
  par$Estimates <- round(par$Estimates,4)
  par$'t-stat' <- round(par$Estimates/err,2)
  par$Lower <- round(par$Estimates-1.96*err,4); par$Upper <- round(par$Estimates+1.96*err,4)
  
  rownames(par) <- c("delta", paste("rho",1:lag, sep=""), "sigma")
  tmp <- ""
  for (i in 1:lag){
    tmp <- paste(tmp,"rho",i,"*y_t-",i," + ", sep="");
  }
  print(paste("AR(",lag,")"," : y_t = delta + ",tmp ,"eps,    eps ~ iid N(0,sigma^2)",sep=""))
  print("________________________________________________________________________________")
  print(par)
  print("________________________________________________________________________________")
  yfit <- delta + ly%*%rho
  res <- y-yfit
  porttest <- Box.test(res,lag=10,type=(c("Ljung-Box")))
  print(paste("H0: No autoccorelation in residuals", "p-value:",round(porttest[["p.value"]],3), sep = " "))
  if(porttest[["p.value"]]<0.05){
    print("... H0 is rejected!")
  } else{
    print("... Cannot rejct H0")
  }
  return(list(Estimates = par, Fitted = yfit[(lag+1):length(yfit)], Residuals = res[(lag+1):length(yfit)], Volatility = sigma, Portmanteau = porttest))
}

#########################
########## MS ###########
#########################
MS <- function(y, lag = 1, regime = 2){
  regime <- as.integer(regime)
  rho <- matrix(pacf(y,lag, plot = F)[["acf"]], lag, 1);
  delta <- mean(y)
  sigma <- matrix(sqrt(var(y)), regime, 1);
  P <- rep(1/regime,regime*(regime-1));
  par <- c(delta, rho, sigma, P)
  
  ly <- matrix(NA, length(y), lag)
  for (i in 1:lag){
    ly[,i] <- lag(y,i)
  }
  
  mle <- function(par, index = 1){
    delta <- par[1];
    rho <- matrix(par[2:(1+lag)],lag,1);
    sigma <- matrix(par[(2+length(rho)):(1+length(rho)+regime)],regime,1);
    P <- par[(1+length(delta)+length(rho)+length(sigma)):(length(par))]
    P <- diag(length(P))*P
    
    for (j in 1:dim(P)[2]){
      for (i in 1:dim(P)[2])
        if (i != j){
          P[i,j] <- 1-diag(P)[j]
        }
    }
    
    meaneq <- y-ly%*%rho
    p_Q <- matrix(c(eigen(P)$values[regime],1-eigen(P)$values[regime]), regime, length(y))
    f_Q <- p_Q
    f <- t(matrix(0,regime, length(y)))
    l <- 0
    for (t in (lag+1):length(y)){
      for (i in 1:regime){
        f[t,i] <- dnorm(meaneq[t], delta, sigma[i])
      }
      p_Q[,t] <- P%*%f_Q[,t-1]
      l[t] <- t(p_Q[,t])%*%matrix(f[t,])
      f_Q[,t] <- (p_Q[,t]*f[t,])/l[t]
    }
    lf <- sum(log(l[(lag+1):length(y)]))
    
    if (index == 1){
      return(-lf)
    }
    
    if (index == 2){
      return(p_Q)
    }
    
    if (index == 3){
      return(f_Q)
    }
    
    if (index == 4){
      return(P)
    }
    if (index == 5){
      return(sigma)
    }
  }
  
  est <- optim(par,mle, hessian = TRUE)
  p_Q <- mle(est$par, index = 2); f_Q <- mle(est$par, index = 3); P <- mle(est$par, index = 4); sigma <- mle(est$par, index = 5); 
  s_Q <- f_Q
  
  for (h in length(y):2){
    s_Q[,h-1] <- matrix(f_Q[,h-1])*t(P)%*%matrix(s_Q[,h]/p_Q[,h])
  }
  
  par <- est$par
  par <- as.data.frame(est[["par"]]);names(par) <- c("Estimates")
  fisher <- solve(est$hessian); err <- c(sqrt(diag(fisher)));
  par$Estimates <- round(par$Estimates,4)
  par$'t-stat' <- round(par$Estimates/err,2)
  par$Lower <- round(par$Estimates-1.96*err,4); par$Upper <- round(par$Estimates+1.96*err,4)
  rownames(par) <- c("delta", paste("rho",1:lag, sep=""), paste("sigma",1:regime, sep=""),paste("P",1:regime, sep=""))
  print("________________________________________________________________________________")
  print(par)
  print("________________________________________________________________________________")
  yfit <- delta + ly%*%rho
  res <- y-yfit
  porttest <- Box.test(res,lag=5,type=(c("Ljung-Box")))
  print(paste("H0: No autoccorelation in residuals", "p-value:",round(porttest[["p.value"]],3), sep = " "))
  if(porttest[["p.value"]]<0.05){
    print("... H0 is rejected!")
  } else{
    print("... Cannot rejct H0")
  }
  return(list(Estimates = par, Regimes = s_Q,Fitted = yfit[(lag+1):length(yfit)], Residuals = res[(lag+1):length(yfit)], Volatility = sigma, Portmanteau = porttest))
}

#########################
########## OLS ##########
#########################
OLS <- function(y,x, test = FALSE){
  beta <- solve(t(x)%*%x)%*%t(x)%*%y
  yfit <- x%*%beta
  res <- y-yfit
  sigma <- t(res)%*%res/(length(y)-ncol(beta))
  err <- rep(sigma,ncol(beta))*solve(t(x)%*%x)
  err <- sqrt(diag(err))
  parest <- c(beta,sigma,err)
  par <- as.data.frame(c(beta,sigma)); names(par) <- c("Estimates")
  par$'t-stat' <- c(round(par$Estimates[1:length(beta)]/err,2),NA)
  par$Lower <- c(round(par$Estimates[1:length(beta)]-1.96*err,4),NA); par$Upper <- c(round(par$Estimates[1:length(beta)]+1.96*err,4),NA)
  par$Estimates <- round(par$Estimates,4)
  rownames(par) <- c(paste("beta",1:(length(beta)), sep=""),"sigma")
  print(paste("OLS : y = x%*%beta + eps,    E(eps|x) = 0 ",sep=""))
  print("________________________________________________________________________________")
  print(par)
  print("________________________________________________________________________________")
  if(test == TRUE){
    #porttest <- Box.test(res,lag=5,type=(c("Ljung-Box")))
    #print(paste("H0: No autoccorelation in residuals", "p-value:",round(porttest[["p.value"]],3), sep = " "))
    #if(porttest[["p.value"]]<0.05){
    #  print("... H0 is rejected!")
    #} else{
    #  print("... Cannot rejct H0")
    #}
  }
  return(list(Estimates = par,Fitted = yfit, Residuals = res, par = parest))
}

#########################
######### ARCH ##########
#########################
ARCH <- function(y, AR_lag = 1, ARCH_lag=1){
  rho <- matrix(pacf(y, AR_lag, plot = F)[["acf"]], AR_lag, 1)
  delta <- mean(y)/(1-sum(rho))
  alfa <- matrix(pacf(y^2, ARCH_lag, plot = F)[["acf"]], ARCH_lag, 1)
  omega <- var(y)/(1-sum(alfa))
  par <- c(delta, rho, omega, alfa)
  
  ly <- matrix(NA, length(y), AR_lag)
  for (i in 1:AR_lag){
    ly[,i] <- lag(y,i)
  }
  
  mle <- function(par){
    delta <- par[1]; rho <- matrix(par[2:(1 + AR_lag)],AR_lag,1); 
    omega <- par[((2 + AR_lag))]
    alfa <- matrix(par[(length(par)-ARCH_lag+1):length(par)],ARCH_lag,1)
    meaneq <- y-ly%*%rho-delta
    lmeaneq <- matrix(NA, length(meaneq), ARCH_lag)
    for (i in 1:ARCH_lag){
      lmeaneq[,i] <- lag(meaneq,i)
    }
    vareq <- omega + lmeaneq^2%*%alfa
    
    f <- dnorm(meaneq, 0, vareq)
    lf <- sum(log(f[(ARCH_lag+AR_lag+1):length(f)]))
    return(-lf)
  }
  
  est <- optim(par,mle, hessian = TRUE); par <- as.data.frame(est[["par"]]);names(par) <- c("Estimates")
  fisher <- solve(est$hessian); err <- c(sqrt(diag(fisher)));
  par$Estimates <- round(par$Estimates,4)
  par$'t-stat' <- round(par$Estimates/err,2)
  par$Lower <- round(par$Estimates-1.96*err,4); par$Upper <- round(par$Estimates+1.96*err,4)
  
  row.names(par) <- c("delta", paste("rho",1:AR_lag, sep=""), "omega", paste("alfa",1:ARCH_lag, sep=""))
  print("________________________________________________________________________________")
  print(par)
  print("________________________________________________________________________________")
  yfit <- delta + ly%*%rho
  res <- y-yfit
  meaneq <- y-ly%*%rho-delta
  sigma <- omega + meaneq^2%*%alfa
  porttest <- Box.test(res,lag=10,type=(c("Ljung-Box")))
  print(paste("H0: No autoccorelation in residuals", "p-value:",round(porttest[["p.value"]],3), sep = " "))
  if(porttest[["p.value"]]<0.05){
    print("... H0 is rejected!")
  } else{
    print("... Cannot rejct H0")
  }
  return(list(Estimates = par, Fitted = yfit[(AR_lag+1):length(yfit)], Residuals = res[(AR_lag+1):length(yfit)],Volatility = sqrt(sigma[(AR_lag+1):length(sigma)]), Portmanteau = porttest))
}

#########################
########## MS3 ##########
#########################
MS3 <- function(y, lag = 1, regime = 3, Pmatrix = c(0.985,0.015,0.02,0.8,0.0001,0.4)){
  regime <- as.integer(regime)
  rho <- matrix(pacf(y,lag, plot = F)[["acf"]], lag, 1);
  delta <- mean(y)
  sigma <- matrix(sqrt(var(y)), regime, 1);
  #P <- c(0.985,0.015,0.02,0.8,0.0001,0.4);
  #P <- c(0.982,0.015,0.02,0.75,0.0008,0.45);
  #P <- c(0.987,0.011,0.02,0.8,0.001,0.4);
  P <- Pmatrix
  par <- c(delta, rho, sigma, P)
  
  ly <- matrix(NA, length(y), lag)
  for (i in 1:lag){
    ly[,i] <- lag(y,i)
  }
  
  mle <- function(par, index = 1){
    delta <- par[1];
    rho <- matrix(par[2:(1+lag)],lag,1);
    sigma <- matrix(par[(2+length(rho)):(1+length(rho)+regime)],regime,1);
    
    P <- matrix(c(par[6],par[7], 1-par[6]-par[7],par[8],par[9], 1-par[8]-par[9],par[10],par[11], 1-par[10]-par[11]),3,3)
    
    meaneq <- y-ly%*%rho
    p_Q <- matrix(c(eigen(P)$values[regime],1-eigen(P)$values[regime]), regime, length(y))
    f_Q <- p_Q
    f <- t(matrix(0,regime, length(y)))
    l <- 0
    for (t in (lag+1):length(y)){
      for (i in 1:regime){
        f[t,i] <- dnorm(meaneq[t], delta, sigma[i])
      }
      p_Q[,t] <- P%*%f_Q[,t-1]
      l[t] <- t(p_Q[,t])%*%matrix(f[t,])
      f_Q[,t] <- (p_Q[,t]*f[t,])/l[t]
    }
    lf <- sum(log(l[(lag+1):length(y)]))
    
    if (index == 1){
      return(-lf)
    }
    
    if (index == 2){
      return(p_Q)
    }
    
    if (index == 3){
      return(f_Q)
    }
    
    if (index == 4){
      return(P)
    }
    if (index == 5){
      return(sigma)
    }
  }
  
  est <- optim(par,mle, hessian = TRUE)
  
  p_Q <- mle(est$par, index = 2); f_Q <- mle(est$par, index = 3); P <- mle(est$par, index = 4); sigma <- mle(est$par, index = 5); 
  s_Q <- f_Q
  
  for (h in length(y):2){
    s_Q[,h-1] <- matrix(f_Q[,h-1])*t(P)%*%matrix(s_Q[,h]/p_Q[,h])
  }
  
  par <- est$par
  par <- as.data.frame(est[["par"]]);names(par) <- c("Estimates")
  fisher <- solve(est$hessian); err <- c(sqrt(diag(fisher)));
  par$Estimates <- round(par$Estimates,4)
  par$'t-stat' <- round(par$Estimates/err,2)
  par$Lower <- round(par$Estimates-1.96*err,4); par$Upper <- round(par$Estimates+1.96*err,4)
  rownames(par) <- c("delta", paste("rho",1:lag, sep=""), paste("sigma",1:regime, sep=""),paste("P",1:(regime*(regime-1)), sep=""))
  print("________________________________________________________________________________")
  print(par)
  print("________________________________________________________________________________")
  yfit <- delta + ly%*%rho
  res <- y-yfit
  porttest <- Box.test(res,lag=5,type=(c("Ljung-Box")))
  print(paste("H0: No autoccorelation in residuals", "p-value:",round(porttest[["p.value"]],3), sep = " "))
  if(porttest[["p.value"]]<0.05){
    print("... H0 is rejected!")
  } else{
    print("... Cannot rejct H0")
  }
  return(list(Estimates = par, Regimes = s_Q,Fitted = yfit[(lag+1):length(yfit)], Residuals = res[(lag+1):length(yfit)], Volatility = sigma, Portmanteau = porttest))
}

#########################
########## VAR ###########
#########################
VAR <- function(y, lag = 1){
  vars::VAR(y, p =1 , type = c("const"))
  est <- vars::VAR(y, p =1 , type = c("const"))
  print("Fitted model")
  print(Bcoef(est))
  print("Variance-covariance matrix")
  print(summary(est)$covres)
  return(list(Estimates = Bcoef(est), covvar = summary(est)$covres, correlation = summary(est)$corres))
}


#########################
######### MGARCH #########
#########################
MGARCH <- function(y, lag = 1){
  garch <- ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")
  model <- dccspec(uspec = multispec(replicate(4,garch)), dccOrder = c(1,1), distribution = "mvnorm")
  est <- dccfit(model, data = y)
  names(est@model)
  return(list(VarCov = est@mfit[["H"]]))
}


#########################
###### Kalman filter ####
#########################

#Function Kalman Filter

KalmanFilter <- function(y,x,t, ols_window = 20){
  y <- matrix(y)
  x <- matrix(x)
  k <- dim(x)[2]
  N <- dim(y)[1]
  
  est <- OLS(y,x)
  beta_ols <- est$Estimates[1,k]
  
  #MLE starting values
  omega <- sd(y)
  omega <- 0.4
  sigma <- ifelse(est$Estimates[2,k]<0.1, 0.2, est$Estimates[2,k])
  gamma <- 1
  
  par <- c(omega, sigma)
  
  #Kalman filter starting values
  pred_beta <- matrix(NA,N-1,1); pred_beta_variance <- matrix(NA,N-1,1)
  kalman_gain <- matrix(NA,N-1,1)
  measurement_error <- matrix(NA,N-1,1); measurement_variance <- matrix(NA,N-1,1)
  
  llcontribution <- matrix(0,N-1,1)
  
  filt_beta <- matrix(beta_ols,N,1) 
  filt_beta_variance <- matrix(sigma^2,N,1) 
  
  llf <- function(par, index = 1){
    
    omega <- par[1]; sigma <- par[2]
    
    for (t in 2:N){
      
      pred_beta[t] <- gamma*filt_beta[t-1] #E(beta_t | y_1:t-1) = E(gamma*beta_t-1+eta_t | y_t:t-1)
      pred_beta_variance[t] <- gamma^2*filt_beta_variance[t-1]+sigma^2 #E(beta_t-E(beta_t | y_t:t-1) | y_t:t-1)^2
      
      measurement <- pred_beta[t]*x[t] #E(y_t|y_t:t-1) = E(beta_t|y_1:t-1)*x_t
      measurement_error[t] <- y[t]-measurement #y_t-E(y_t|y_1:t-1)
      measurement_variance[t] <- pred_beta_variance[t]*x[t]^2+omega^2 #E(y_t|y_t:t-1)^2
      kalman_gain[t] <- pred_beta_variance[t]*x[t]/measurement_variance[t]  # E((y_t-E(y_t|y_1:t-1))*(beta_t-E(beta_t|y_1:t-1)) | y_1:t-1)/E(y_t|y_1:t-1)^2
      
      llcontribution[t] <- log(dnorm(measurement_error[t], 0, measurement_variance[t]))
      
      filt_beta[t] <- pred_beta[t]+kalman_gain[t]*measurement_error[t] #E(beta_t | y_1:t)
      filt_beta_variance[t] <- (1-kalman_gain[t]*x[t])*pred_beta_variance[t] #E(beta_t| y_1:t)^2
    }
    
    loglikelihood <- sum(llcontribution)
    
    if(index == 1){
      return(-loglikelihood)
    }
    
    if(index == 2){
      return(filt_beta)
    }
  }
  
  llf(par)
  est <- optim(par,llf, hessian = TRUE)
  par <- est$par
  Estimation <- data.frame(par)
  rownames(Estimation) <- c("Omega", "Sigma")
  omega <- est$par[1]
  sigma <- est$par[2]
  parest <- c(omega, sigma)
  
  fisher <- solve(est$hessian); err <- c(sqrt(diag(fisher)));
  
  lower <- c(est$par[1]-1.96*err[1], est$par[2]-1.96*err[2])
  upper <- c(est$par[1]+1.96*err[1], est$par[2]+1.96*err[2])
  
  Estimation$'t-stat' <- est$par/err
  Estimation$Lower <- round(est$par-1.96*err,4); Estimation$Upper <- round(est$par+1.96*err,4)
  print("##################################################")
  print("##################################################")
  print("Kalman Filter")
  print("Measurement eq.: Y_t = Beta_t*X_t + Omega*Eps_t")
  print("State eq.: Beta_t = Gamma*Beta_t-1 + Sigma*Eta_t")
  print("__________________________________________________")
  print(Estimation)
  
  beta_kf <- llf(par,index = 2)
  beta_kf_lower <- matrix(beta_kf,length(beta_kf))
  beta_kf_upper <- matrix(beta_kf,length(beta_kf))
  for (i in 2:length(beta_kf)){
    beta_kf_lower[i] <- qnorm(0.025, mean = beta_kf[i-1], sd = abs(sigma))
    beta_kf_upper[i] <- qnorm(0.975, mean = beta_kf[i-1], sd = abs(sigma))
  }
  
  y_kf <- beta_kf*x
  y_ols <- beta_ols*x
  
  
  window <- ols_window
  beta_ols_rolling <- matrix(NA,length(y),1)
  for (j in 1:(length(y)-window)){
    
    est <- OLS(y[j:(j+window)],
               x[j:(j+window)])
    
    beta_ols_rolling[((j+window-1)):(j+window)] <- est$Estimates[1,1]
  }
  
  
  ts.plot <- ggplot(data.frame(t, beta_kf, beta_ols, beta_kf_lower, beta_kf_upper), aes(x=t)) +
    geom_line(aes(y = beta_kf, col = "Kalman filter"), size = 1.5) +
    geom_line(aes(y = beta_ols, col = "OLS"), size = 1) +
    geom_ribbon(aes(ymin = beta_kf_lower, ymax = beta_kf_upper, x=t, fill = "CF-interval"), alpha = 0.2)+
    geom_line(aes(y = beta_ols_rolling, col = paste("Rol_OLS - window: ", window, sep ="")), size = 1) +
    ggtitle("Estimated beta: y_t = beta_t*x_t + eps_t") +
    theme(plot.title = element_text(hjust = 0.5)) +labs(x = "", y = "", col ="") +
    theme_light() +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
    ylim(min(c(beta_kf, beta_ols_rolling))-.5, max(c(beta_kf, beta_ols_rolling))+.5)
  ts.plot
  
  cat("\f")
  print(Estimation)
  output <- data.frame(t, beta_kf, beta_kf_lower, beta_kf_upper, beta_ols_rolling)
  return(list(output, ts.plot, parest))
}


KalmanFilter_rol <- function(y,x,t, kf_window = 30, ols_window = 20){
  
  y <- matrix(y)
  x <- matrix(x)
  k <- dim(x)[2]
  N <- dim(y)[1]
  
  kf <- KalmanFilter(y,x,t ,30)
  cat("\f")
  kf[[3]]
  
  
  
  est <- OLS(y,x)
  beta_ols <- est$Estimates[1,k]
  
  #MLE starting values
  omega <- sd(y)
  omega <- 0.4
  sigma <- est$Estimates[2,k]
  gamma <- 1
  
  par <- c(omega, sigma)
  
  #Kalman filter starting values
  pred_beta <- matrix(NA,N-1,1); pred_beta_variance <- matrix(NA,N-1,1)
  kalman_gain <- matrix(NA,N-1,1)
  measurement_error <- matrix(NA,N-1,1); measurement_variance <- matrix(NA,N-1,1)
  
  llcontribution <- matrix(0,N-1,1)
  
  filt_beta <- matrix(beta_ols,N,1) 
  filt_beta_variance <- matrix(sigma^2,N,1) 
  
  llf <- function(par, index = 1){
    
    omega <- par[1]; sigma <- par[2]
    
    for (t in 2:N){
      
      pred_beta[t] <- gamma*filt_beta[t-1] #E(beta_t | y_1:t-1) = E(gamma*beta_t-1+eta_t | y_t:t-1)
      pred_beta_variance[t] <- gamma^2*filt_beta_variance[t-1]+sigma^2 #E(beta_t-E(beta_t | y_t:t-1) | y_t:t-1)^2
      
      measurement <- pred_beta[t]*x[t] #E(y_t|y_t:t-1) = E(beta_t|y_1:t-1)*x_t
      measurement_error[t] <- y[t]-measurement #y_t-E(y_t|y_1:t-1)
      measurement_variance[t] <- pred_beta_variance[t]*x[t]^2+omega^2 #E(y_t|y_t:t-1)^2
      kalman_gain[t] <- pred_beta_variance[t]*x[t]/measurement_variance[t]  # E((y_t-E(y_t|y_1:t-1))*(beta_t-E(beta_t|y_1:t-1)) | y_1:t-1)/E(y_t|y_1:t-1)^2
      
      llcontribution[t] <- log(dnorm(measurement_error[t], 0, measurement_variance[t]))
      
      filt_beta[t] <- pred_beta[t]+kalman_gain[t]*measurement_error[t] #E(beta_t | y_1:t)
      filt_beta_variance[t] <- (1-kalman_gain[t]*x[t])*pred_beta_variance[t] #E(beta_t| y_1:t)^2
    }
    
    loglikelihood <- sum(llcontribution)
    
    if(index == 1){
      return(-loglikelihood)
    }
    
    if(index == 2){
      return(filt_beta)
    }
  }
  
  llf(par)
  est <- optim(par,llf, hessian = TRUE)
  par <- est$par
  Estimation <- data.frame(par)
  rownames(Estimation) <- c("Omega", "Sigma")
  sigma <- est$par[2]
  
  fisher <- solve(est$hessian); err <- c(sqrt(diag(fisher)));
  
  lower <- c(est$par[1]-1.96*err[1], est$par[2]-1.96*err[2])
  upper <- c(est$par[1]+1.96*err[1], est$par[2]+1.96*err[2])
  
  Estimation$'t-stat' <- est$par/err
  Estimation$Lower <- round(est$par-1.96*err,4); Estimation$Upper <- round(est$par+1.96*err,4)
  print("##################################################")
  print("##################################################")
  print("Kalman Filter")
  print("Measurement eq.: Y_t = Beta_t*X_t + Omega*Eps_t")
  print("State eq.: Beta_t = Gamma*Beta_t-1 + Sigma*Eta_t")
  print("__________________________________________________")
  print(Estimation)
  
  beta_kf <- llf(par,index = 2)
  beta_kf_lower <- matrix(beta_kf,length(beta_kf))
  beta_kf_upper <- matrix(beta_kf,length(beta_kf))
  for (i in 2:length(beta_kf)){
    beta_kf_lower[i] <- qnorm(0.025, mean = beta_kf[i-1], sd = sigma)
    beta_kf_upper[i] <- qnorm(0.975, mean = beta_kf[i-1], sd = sigma)
  }
  
  y_kf <- beta_kf*x
  y_ols <- beta_ols*x
  
  
  window <- ols_window
  beta_ols_rolling <- matrix(NA,length(y),1)
  for (j in 1:(length(y)-window)){
    
    est <- OLS(y[j:(j+window)],
               x[j:(j+window)])
    
    beta_ols_rolling[((j+window-1)):(j+window)] <- est$Estimates[1,1]
  }
  
  
  ts.plot <- ggplot(data.frame(t, beta_kf, beta_ols, beta_kf_lower, beta_kf_upper), aes(x=t)) +
    geom_line(aes(y = beta_kf, col = "Kalman filter"), size = 1.5) +
    geom_line(aes(y = beta_ols, col = "OLS"), size = 1) +
    geom_ribbon(aes(ymin = beta_kf_lower, ymax = beta_kf_upper, x=t, fill = "CF-interval"), alpha = 0.2)+
    geom_line(aes(y = beta_ols_rolling, col = paste("Rol_OLS - window: ", window, sep ="")), size = 1) +
    ggtitle("Estimated beta: y_t = beta_t*x_t + eps_t") +
    theme(plot.title = element_text(hjust = 0.5)) +labs(x = "", y = "", col ="") +
    theme_light() +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
    ylim(min(c(beta_kf, beta_ols_rolling))-.5, max(c(beta_kf, beta_ols_rolling))+.5)
  ts.plot
  
  cat("\f")
  print(Estimation)
  output <- data.frame(t, beta_kf, beta_kf_lower, beta_kf_upper, beta_ols_rolling)
  return(list(output, ts.plot))
}


KF <- function(y,x,t, ols_window = 30){
  y <- matrix(y)
  k <- dim(x)[2]
  N <- dim(y)[1]
  x <- matrix(x,N,k)
  
  est <- OLS(y,x)
  beta_ols <- est$par[1:k]
  errors <- est$par[(length(est$par)-k+1):length(est$par)]
  
  #MLE starting values
  omega <- sd(y)
  omega <- 0.4
  sigma <- diag(errors,k)
  gamma <- diag(1,k)
  
  par <- c(omega, diag(sigma))
  
  #Kalman filter starting values
  pred_beta <- matrix(NA,k,k); pred_beta_variance <- matrix(NA,k,k)
  
  kalman_gain <- matrix(NA,N,k)
  measurement_error <- matrix(NA,N-1,1); measurement_variance <- matrix(NA,N-1,1)
  
  llcontribution <- matrix(0,N-1,1)
  
  filt_beta <- matrix(NA,N,k); filt_beta[1,1] <- beta_ols[1]; filt_beta[1,2] <- beta_ols[2]
  filt_beta_variance <- matrix(NA,k,k); filt_beta_variance[1:k,1:k] <- t(sigma)%*%sigma
  
  llf <- function(par, index = 1){
    
    omega <- par[1]; sigma <- diag(c(par[2],par[3]),k)
    
    for (t in 2:N){
      
      pred_beta <- gamma%*%filt_beta[t-1,] #E(beta_t | y_1:t-1) = E(gamma*beta_t-1+eta_t | y_t:t-1)
      pred_beta_variance <- t(gamma)%*%gamma%*%filt_beta_variance+t(sigma)%*%sigma #E(beta_t-E(beta_t | y_t:t-1) | y_t:t-1)^2
      
      measurement <- t(pred_beta)%*%x[t,] #E(y_t|y_t:t-1) = E(beta_t|y_1:t-1)*x_t
      measurement_error[t] <- y[t]-measurement #y_t-E(y_t|y_1:t-1)
      measurement_variance[t] <- t(x[t,])%*%pred_beta_variance%*%x[t,]+t(omega)%*%omega #E(y_t|y_t:t-1)^2
      kalman_gain[t,] <- pred_beta_variance%*%x[t,]%*%solve(measurement_variance[t])  # E((y_t-E(y_t|y_1:t-1))*(beta_t-E(beta_t|y_1:t-1)) | y_1:t-1)/E(y_t|y_1:t-1)^2
      
      llcontribution[t] <- log(dnorm(measurement_error[t], 0, measurement_variance[t]))
      
      filt_beta[t,] <- pred_beta+kalman_gain[t,]*measurement_error[t] #E(beta_t | y_1:t)
      filt_beta_variance<- (diag(1,k,k)-diag(kalman_gain[t,]*x[t,]))%*%pred_beta_variance#E(beta_t| y_1:t)^2
    }
    
    loglikelihood <- sum(llcontribution)
    
    if(index == 1){
      return(-loglikelihood)
    }
    
    if(index == 2){
      return(filt_beta)
    }
  }
  
  llf(par)
  est <- optim(par,llf, hessian = TRUE)
  par <- est$par
  Estimation <- data.frame(par)
  rownames(Estimation) <- c("Omega", "Sigma1", "Sigma2")
  omega <- est$par[1]
  sigma <- est$par[2:(k+1)]
  parest <- c(omega, sigma)
  
  fisher <- solve(est$hessian); err <- c(sqrt(diag(fisher)));
  
  lower <- c(est$par[1]-1.96*err[1], est$par[2]-1.96*err[2], est$par[3]-1.96*err[3])
  upper <- c(est$par[1]+1.96*err[1], est$par[2]+1.96*err[2], est$par[3]-1.96*err[3])
  
  Estimation$'t-stat' <- est$par/err
  Estimation$Lower <- round(est$par-1.96*err,4); Estimation$Upper <- round(est$par+1.96*err,4)
  print("##################################################")
  print("##################################################")
  print("Kalman Filter")
  print("Measurement eq.: Y_t = Beta_t*X_t + Omega*Eps_t")
  print("State eq.: Beta_t = Gamma*Beta_t-1 + Sigma*Eta_t")
  print("__________________________________________________")
  print(Estimation)
  
  beta_kf <- llf(par,index = 2)
  beta_kf_lower <- matrix(beta_kf,nrow(beta_kf),k)
  beta_kf_upper <- matrix(beta_kf,nrow(beta_kf),k)
  for (i in 2:nrow(beta_kf)){
    beta_kf_lower[i,] <- qnorm(0.025, mean = beta_kf[i-1,], sd = abs(sigma))
    beta_kf_upper[i,] <- qnorm(0.975, mean = beta_kf[i-1,],  sd = abs(sigma))
  }
  
  window <- ols_window
  beta_ols_rolling <- matrix(NA,length(y),2)
  for (j in 1:(length(y)-window)){
    
    est <- OLS(y[j:(j+window)],
               x[j:(j+window),])
    
    beta_ols_rolling[((j+window-1)):(j+window),1] <-  est$par[1]
    beta_ols_rolling[((j+window-1)):(j+window),2] <-  est$par[2]
  }
  
  
  cat("\f")
  print(Estimation)
  output <- data.frame(t, beta_kf, beta_kf_lower, beta_kf_upper, beta_ols_rolling)
  colnames(output) <- c("t", "beta_kf1", "beta_kf2", "beta_kf_lower1", "beta_kf_lower2", "beta_kf_upper1", "beta_kf_upper2","beta_ols_rolling1","beta_ols_rolling2")
  return(list(output, ts.plot, parest))
}


#########################
########## PCA ##########
#########################
PCA <- function(Data, Loadings = 1, EWMA = FALSE, Lambda = 0.92){
  
  if(EWMA == TRUE){
    if(Lambda > 1 | Lambda <0){
      return("Lambda should be in (0,1)")
    }
    w <- (1-Lambda)*Lambda^(0:(nrow(Data)-1))
    Data <- w*Data
  }
  
  #Re-center
  Data <- Data-colMeans(Data)
  
  # Determine covariance matrix
  A <- cov(Data)
  
  # Find eigenvalues
  A <- eigen(A)
  
  #prcomp(Data)$rotation[,'PC1'] alternatively
  
  # Components 
  C <- as.data.frame(A$vectors[,1:Loadings])
  colnames(C) <- paste("Loadings ", 1:Loadings)
  return(C)
}




KF4 <-   function(y,x,t, ols_window = 30){
  y <- matrix(y)
  k <- dim(x)[2]
  N <- dim(y)[1]
  x <- matrix(x,N,k)
  
  est <- OLS(y,x)
  beta_ols <- c(est$par[1],est$par[2],est$par[3],est$par[4])
  errors <- est$par[(length(est$par)-k+1):length(est$par)]; errors[3] <- errors[2]; errors[4] <- errors[2]
  
  #MLE starting values
  omega <- sd(y)
  #omega <- 0.4
  sigma <- diag(errors,k)
  gamma <- diag(1,k)
  
  par <- c(omega, diag(sigma))
  
  #Kalman filter starting values
  pred_beta <- matrix(NA,k,k); pred_beta_variance <- matrix(NA,k,k)
  
  kalman_gain <- matrix(NA,N,k)
  measurement_error <- matrix(NA,N-1,1); measurement_variance <- matrix(NA,N-1,1)
  
  llcontribution <- matrix(0,N-1,1)
  
  filt_beta <- matrix(NA,N,k); filt_beta[1,1] <- beta_ols[1]; filt_beta[1,2] <- beta_ols[2]; filt_beta[1,3] <- beta_ols[3]; filt_beta[1,4] <- beta_ols[4]
  filt_beta_variance <- matrix(NA,k,k); filt_beta_variance[1:k,1:k] <- t(sigma)%*%sigma
  
  llf <- function(par, index = 1){
    
    omega <- par[1]; sigma <- diag(c(par[2],par[3], par[4], par[5]),k)
    
    for (t in 2:N){
      
      pred_beta <- gamma%*%filt_beta[t-1,] #E(beta_t | y_1:t-1) = E(gamma*beta_t-1+eta_t | y_t:t-1)
      pred_beta_variance <- t(gamma)%*%gamma%*%filt_beta_variance+t(sigma)%*%sigma #E(beta_t-E(beta_t | y_t:t-1) | y_t:t-1)^2
      
      measurement <- t(pred_beta)%*%x[t,] #E(y_t|y_t:t-1) = E(beta_t|y_1:t-1)*x_t
      measurement_error[t] <- y[t]-measurement #y_t-E(y_t|y_1:t-1)
      measurement_variance[t] <- t(x[t,])%*%pred_beta_variance%*%x[t,]+t(omega)%*%omega #E(y_t|y_t:t-1)^2
      kalman_gain[t,] <- pred_beta_variance%*%x[t,]%*%solve(measurement_variance[t])  # E((y_t-E(y_t|y_1:t-1))*(beta_t-E(beta_t|y_1:t-1)) | y_1:t-1)/E(y_t|y_1:t-1)^2
      
      llcontribution[t] <- log(dnorm(measurement_error[t], 0, measurement_variance[t]))
      
      filt_beta[t,] <- pred_beta+kalman_gain[t,]*measurement_error[t] #E(beta_t | y_1:t)
      filt_beta_variance<- (diag(1,k,k)-diag(kalman_gain[t,]*x[t,]))%*%pred_beta_variance#E(beta_t| y_1:t)^2
    }
    
    loglikelihood <- sum(llcontribution)
    
    if(index == 1){
      return(-loglikelihood)
    }
    
    if(index == 2){
      return(filt_beta)
    }
  }
  
  llf(par)
  est <- optim(par,llf, hessian = TRUE)
  par <- est$par
  Estimation <- data.frame(par)
  rownames(Estimation) <- c("Omega", "Sigma1", "Sigma2", "Sigma3", "Sigma4")
  omega <- est$par[1]
  sigma <- est$par[2:(k+1)]
  parest <- c(omega, sigma)
  
  
  print("##################################################")
  print("##################################################")
  print("Kalman Filter")
  print("Measurement eq.: Y_t = Beta_t*X_t + Omega*Eps_t")
  print("State eq.: Beta_t = Gamma*Beta_t-1 + Sigma*Eta_t")
  print("__________________________________________________")
  print(Estimation)
  
  beta_kf <- llf(par,index = 2)
  
  window <- ols_window
  
  beta_kf_lower <- matrix(beta_kf,nrow(beta_kf),k)
  beta_kf_upper <- matrix(beta_kf,nrow(beta_kf),k)
  for (i in 2:nrow(beta_kf)){
    beta_kf_lower[i,] <- qnorm(0.025, mean = beta_kf[i-1,], sd = abs(sigma))
    beta_kf_upper[i,] <- qnorm(0.975, mean = beta_kf[i-1,],  sd = abs(sigma))
  }
  
  window <- ols_window
  beta_ols_rolling <- matrix(NA,length(y),k)
  for (j in 1:(length(y)-window)){
    
    est <- OLS(y[j:(j+window)],
               x[j:(j+window),])
    
    beta_ols_rolling[((j+window-1)):(j+window),1] <-  est$par[1]
    beta_ols_rolling[((j+window-1)):(j+window),2] <-  est$par[2]
    beta_ols_rolling[((j+window-1)):(j+window),3] <-  est$par[3]
    beta_ols_rolling[((j+window-1)):(j+window),4] <-  est$par[4]
  }
  
  
  cat("\f")
  print(Estimation)
  output <- data.frame(t, beta_kf[,1], beta_kf_lower[,1], beta_kf_upper[,1], beta_ols_rolling[,1])
  colnames(output) <- c("t", "beta_kf1", "beta_kf_lower","beta_kf_upper","beta_ols_rolling")
  return(list(output))
}


