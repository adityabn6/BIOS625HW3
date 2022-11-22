sig_i <- function(pval){
  for( i in 1:length(pval)){
    if (as.numeric(pval[i])<0.001) {
      pval[i]<-paste(pval[i],"***")
    } else if (as.numeric(pval[i])<0.01) {
      pval[i]<-paste(pval[i],"**")
    } else if  (as.numeric(pval[i])<0.05) {
      pval[i]<-paste(pval[i],"*")
    } else if  (as.numeric(pval[i])<0.1) {
      pval[i]<-paste(pval[i],".")
    } else {
      pval[i]
    }
  }

  return(pval)
}
