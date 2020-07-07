#' @title  Continuous Life Annuities

#' @description Calculates the present value of a continuous life annuity.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths and "constant" for constant force of mortality).
#' @param cap A numeric type value. The value of the payment.
#' @export
#' @keywords Continuous Life Annuities
#' @return Returns a numeric value (the actuarial present value).
#' @references Chapter 2 of  Life Contingencies (1952) by Jordan, chapter 5 of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' aCont(35,7,10,0.04,CSO80MANB,1,"constant",1)
#' aCont(23,5,12,0.04,CSO80MANB,1,"UDD",1)
#'


aCont<-function(x,h,n,i=0.04,data,prop=1,assumption="constant",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && i>=0 && prop>0 && cap>0){
    if(n==0){
      axhnc<-0
      return(axhnc)
    }else{
      if(assumption=="constant"){
        axhnc<-(a(x,h,n,1,i,data,prop,"none",1)/2)+(a(x,h+1,n,1,i,data,prop,"none",1)/2)
        return(as.numeric(axhnc))
      }else if(assumption=="UDD"){
        delta<-log(1+i)
        axhnc<-(E(x,h,i,data,prop,"none",1)-E(x,h+n,i,data,prop,"none",1)-ACont.(x,h,n,i,data,prop,"UDD",1))/delta
      }else{
        stop("Check assumption")
      }
    }
    axhnc<-as.numeric(axhnc)
    px1<-axhnc*cap
    return(px1)
  }else{
    stop("Check Values")
  }
}
