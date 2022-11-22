summary_blog <- function(lr){
  x<-lr[[13]]
  y<-lr[[12]]

  print(paste("blog_lm(formula = ",lr[[2]]))
  res <- quantile(lr[[7]])
  names(res) <- c("Min","1Q","Median","3Q","Max")
  print(res)

  estimate <-lr[[3]][,1]
  se <- sqrt(diag(lr[[6]]))
  tvals <- lr[[3]][,1]/sqrt(diag(lr[[6]]))
  pvals <- sapply(tvals,function(i) pt(abs(i),nrow(y)-ncol(x),lower.tail=FALSE)*2)
  fvals <- sig_i(pvals)
  sse <- t(lr[[7]])%*%lr[[7]]
  mse <- lr[[10]]

  print("Coefficients:")
  sum_tbl <- data.frame("Estimate" = estimate,             # Create example data
                        "Std. Error" = se,
                        "t value" = tvals,
                        "Pr(>|t|)" = fvals,check.names = FALSE)

  print(sum_tbl)
  print("---")
  print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  print(paste("Residual standard error: ",sqrt(mse)," on",lr[[8]]," degrees of freedom"))
  print(paste("Multiple R-squared:  ",1-(sse/lr[[11]]),"   Adjusted R-squared:  ",1-(sse/lr[[8]])/(lr[[11]]/(lr[[8]]+lr[[9]]))))

  cat("Call:","\n",
      paste("blog_lm(formula = ",lr[[2]]),"\n\n",
      "Residuals:","\n", sep="")
  print(res)
  print("Coefficients:")
  print(sum_tbl)
  cat("---","\n",
      "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1","\n\n",
      paste("Residual standard error: ",sqrt(mse)," on",lr[[8]]," degrees of freedom"),"\n",
      paste("Multiple R-squared:  ",1-(sse/lr[[11]]),"   Adjusted R-squared:  ",1-(sse/lr[[8]])/(lr[[11]]/(lr[[8]]+lr[[9]]))),"\n",
      sep="")
}
