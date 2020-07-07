#' @title  Continuous Life Insurance

#' @description Calculates the present value of a continuous life insurance.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths and "constant" for constant force of mortality).
#' @param cap A numeric type value. The value of the payment.
#' @export
#' @keywords Continuous Life Insurance
#' @return Returns a numeric (actuarial present value).
#' @references Chapter 3 of  Life Contingencies (1952) by Jordan, chapter 4 of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' ACont.(24,2,10,0.04,CSO80MANB,1,"UDD",1)
#' ACont.(24,2,10,0.04,CSO80MANB,1,"constant",1)
#'
ACont.<-function(x,h,n,i=0.04,data,prop=1,assumption="UDD",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && i>=0 && prop>0){
    if(n==0){
      Axhnc<-0
      return(Axhnc)
    }else{
      delta<-log(1+i)
      if(assumption=="constant"){
        Axhnc<-E(x,h,i,data,prop)-E(x,h+n,i,data,prop,"none",1)-delta*aCont(x,h,n,i,data,prop,"constant",1)
      }else if(assumption=="UDD"){
        Axhnc<-(i/delta)*A.(x,h,n,1,i,data,prop,"none",1)
      }else{
        stop("Check assumption")
      }
    }
    Axhnc<-as.numeric(Axhnc)
    px1<-Axhnc*cap
    return(px1)
  }else{
    stop("Check Values")
  }
}


