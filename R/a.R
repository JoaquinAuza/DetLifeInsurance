#' @title  Life Annuities

#' @description Calculates the present value of a life annuity.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of payments per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age, and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param cap A numeric type value. The annualized value of the payment.
#' @export
#' @keywords Life Annuities
#' @return Returns a numeric value (actuarial present value).
#' @references Chapter 2 of  Life Contingencies (1952) by Jordan, chapter 5 of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' a(20,0,15,1,0.04,CSO58FALB,1,"none",1200)
#' a(23,7,9,1,0.04,GAM71F,1,"none",5000)
#' a(33,3,10,4,0.04,CSO80MANB,1,"constant",3000)
#' a(20,5,10,4,0.04,CSO58MANB,1,"UDD",5000)
#'


a<-function(x,h,n,k=1,i=0.04,data,prop=1,assumption="none",cap=1){
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && i>=0 && prop>0 && cap>0){
    if(n==0){
      axhn<-0
    } else if(k==1){
      axhn<-0
      for(j in h:(h+n-1)){
        axhn<-axhn+E(x,j,i,data,prop,"none",1)
      }
    } else {
      if(assumption=="constant"){
        axhn<-a(x,h,n,1,i,data,prop,"none",1)-((k-1)/(2*k))*(E(x,h,i,data,prop,"none",1)-E(x,h+n,i,data,prop,"none",1))
      } else if(assumption=="UDD"){
        fk<-Rate_converter(i,"i",1,"f",k,"frac")
        axhn<-(E(x,h,i,data,prop,"none",1)-E(x,h+n,i,data,prop,"none",1)-A.(x,h,n,k,i,data,prop,"UDD",1))/fk
      } else {
        stop("Check assumption")
      }
    }
    axhn<-as.numeric(axhn)
    px1<-axhn*cap
    return(px1)
  } else {
    stop("Check values")
  }
}


