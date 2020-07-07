#' @title  Present Value of An Annuity

#' @description Calculates the present value of an annuity.
#' @param l  0 for annuity due or 1 for annuity immediate.
#' @param n A numeric value. The number of payments.
#' @param i A numeric value. The interest rate.
#' @export
#' @keywords Annuity Present Value
#' @return NULL
#' @examples
#' af(0,10,0.03)
#' af(1,15,0.05)
#'


af<-function(l=0,n,i){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(n>0 & i>=0){
    v<-1/(1+i)
    VA<-(1-v^(n))/i
    if(l==1){
      return(VA)
    }else{
      if(l==0){
        VA<-VA*(1+i)
        return(VA)
      }else{
        stop("Check timing")
      }
    }
  } else{
    stop("Check Values")
  }
}
