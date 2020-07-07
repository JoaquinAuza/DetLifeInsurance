#' @title  Future Value of an Annuity

#' @description Calculates the future value  of an annuity.
#' @param l  0 for annuity due or 1 for annuity immediate.
#' @param n A numeric value. The number of payments.
#' @param i A numeric value. The interest rate.
#' @export
#' @keywords Annuity Future Value
#' @return NULL
#' @examples
#' sf(0,12,0.05)
#' sf(1,23,0.04)
#'


sf<-function(l=0,n,i){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(n>0 & i>0){
    VF<-((1+i)^(n)-1)/i
    if(l==1){
      return(VF)
    }else{
      if(l==0){
        VF<-VF*(1+i)
        return(VF)
      }else{
        stop("Check timing")
      }
    }
  } else {
    stop("Check Values")
  }
}
