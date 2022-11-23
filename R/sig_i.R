#' Check significance of p-values
#'
#' \emph{sig_i} assigns a significance value to of p-value in an array of p-values.
#' @param pval Array of p-values.
#' @details sig_i takes an array containing p-values as an argument and assigns significance to each p value.
#' \emph{sig_i} is used to convert non numeric covariates into binary dummy variables
#' @return \emph{sig_i} returns an array, where each element is the p value appended with a symbol denoting it's significant. The meaning of the symbols is as follows:\cr
#' \tabular{rrrrr}{
#'   \strong{Symbol} \tab \strong{Meaning}\cr
#'   \emph{***} \tab p-value<0.001\cr
#'   \emph{**} \tab p-value<0.01\cr
#'   \emph{*} \tab p-value<0.05\cr
#'   \emph{.} \tab p-value<0.1
#' }
#'
#' @examples
#' p_array <- c(0.0004,0.06,0.5,1)
#' sig_i(p_array)
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
