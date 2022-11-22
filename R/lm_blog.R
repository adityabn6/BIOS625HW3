lm_blog <- function(eqn,dataset){
  eqn <- as.character(substitute((eqn)))[2]
  y <- dataset[strsplit(eqn," ~")[[1]][1]]
  x <- dataset[strsplit(eqn," ")[[1]][seq(3,length(strsplit(eqn," ")[[1]]),2)]]

  cat_var <- names(sapply(x,is.numeric)[sapply(x,is.numeric)==FALSE])
  if(length(cat_var)!=0){
    x <- dummy_cols(x,cat_var,remove_selected_columns = TRUE, remove_first_dummy = TRUE)

  }
  Intercept <- rep(1,nrow(x))
  x <- cbind(Intercept,x)
  beta_hat <- solve(as.matrix(t(x))%*%as.matrix(x))%*%as.matrix(t(x))%*%as.matrix(y)

  h <- as.matrix(x)%*%solve(as.matrix(t(x))%*%as.matrix(x))%*%as.matrix(t(x))
  y_hat <- h%*%as.matrix(y)

  e_hat <- as.matrix(y)-y_hat
  sse = t(e_hat)%*%e_hat

  sigma_2_hat <- sse/(nrow(y)-ncol(x))

  var_hat_b <- sigma_2_hat[1,1]*solve(as.matrix(t(x))%*%as.matrix(x))

  dfy <- nrow(y)-ncol(x)
  dfx <- ncol(x)-1

  y_bar <- mean(y[,1])

  mse <- sqrt(sse[1,1]/dfy)
  ssy <- sum((y-y_bar)^2)

  return(list(dataset,eqn,beta_hat,y_hat,sigma_2_hat,var_hat_b,e_hat,dfy,dfx,mse,ssy))



  #Check if n<p
  #Check if x in df
  #Check if y in df
  #Check if x is full rank
}
