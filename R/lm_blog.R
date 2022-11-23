#' Fit a Linear Regression model
#'
#' lm_blog is used to fit Simple Linear Regression and Multiple Linear Regression models with continous or categorical covariates.
#' @param eqn Equation of the model to be fitted. The argument is of the form :Y ~ X1 + X2... + Xn.
#' @param dataset Dataframe containing the covariates and response variable as column names.
#' @details lm_blog takes an equation and a dataframe as arguments to fit a linear regression model represented by the equation.
#' The dataframe should contain the response variable and the covariates present in the equation as column names.\cr The covariates are treated as continous or categorical. lm_blog automatically converts any non numeric data time into dummy variables using one-got encoding.\cr lm_blog then fits the model using the method of least squares
#' @return lm_blog returns a list "lr". The list "lr" contains the following components:\cr
#' \tabular{rrrrr}{
#'   \strong{Element} \tab \strong{Desciption}\cr
#'   \emph{dataset} \tab Equation used to fit the model.\cr
#'   \emph{eqn} \tab Equation used to fit the model.\cr
#'   \emph{beta_hat} \tab Coefficient estimates.\cr
#'   \emph{y_hat} \tab Predicted Y values.\cr
#'   \emph{sigma_2_hat} \tab Estimated model variance.\cr
#'   \emph{var_hat_b} \tab Variance covariance matrix.\cr
#'   \emph{e_hat} \tab Equation used to fit the model.\cr
#'   \emph{dfy} \tab No: of observations - No: of parameters.\cr
#'   \emph{dfx} \tab No: of parameters - 1.\cr
#'   \emph{mse} \tab Mean Squared Error.\cr
#'   \emph{ssy} \tab Total sum of squares.\cr
#'   \emph{y} \tab Response variable.\cr
#'   \emph{x} \tab Covariates.
#' }
#'
#' @examples
#' library(datasets)\crdata(iris)\crlm_blog(Sepal.Length ~ Sepal.Width + Petal.Length + Species,iris)
#' lr <- lm_blog(Sepal.Length ~ Petal.Width + Petal.Length + Species,iris)
#' summary_blog(lr)

lm_blog <- function(eqn,dataset){

  dataset<-na.omit(dataset)
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

  return(list(dataset,eqn,beta_hat,y_hat,sigma_2_hat,var_hat_b,e_hat,dfy,dfx,mse,ssy,x,y))



  #Check if n<p
  #Check if x in df
  #Check if y in df
  #Check if x is full rank
}
