#' @title  Life Insurance

#' @description Calculates the present value of the life insurance.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of fractions per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param cap A numeric type value. The value of the payment.
#' @export
#' @keywords Life Insurance
#' @return Returns a numeric value (actuarial present value).
#' @references Chapter 3 of  Life Contingencies (1952) by Jordan, chapter 4 of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' A.(50,0,8,1,0.04,CSO80MANB,1,"none",1)
#' A.(60,3,10,1,0.04,CSO80MANB,1,"none",1)
#' A.(21,4,7,3,0.04,CSO80MANB,1,"constant",1)
#' A.(23,4,6,12,0.04,CSO80MANB,1,"UDD",1)
#'


A.<-function(x,h,n,k=1,i=0.04,data,prop=1,assumption="none",cap=1){
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && i>=0 && prop>0 && cap>0){
    if(n==0){
      Axhn<-0
    } else if(k==1){
      Axhn<-0
      p<-Survival(x,h,data,prop)
      for(s in h:(h+n-1)){
        Axhn<-Axhn+(1/(1+i))^(s+1)*(as.numeric(data[x+s+1,2])*prop)*p
        p<-p*(1-data[x+s+1,2]*prop)
      }
    } else{
      if(assumption=="constant"){
        fk<-Rate_converter(i,"i",1,"f",k,"frac")
        Axhn<-E(x,h,i,data,prop,"none",1)-E(x,h+n,i,data,prop,"none",1)-fk*a(x,h,n,k,i,data,prop,"constant",1)
      } else if(assumption=="UDD"){
        jk<-Rate_converter(i,"i",1,"j",k,"frac")
        Axhn<-(i/jk)*A.(x,h,n,1,i,data,prop,"none",1)
      } else{
        stop("Check assumption")
      }
    }
    Axhn<-as.numeric(Axhn)
    px1<-Axhn*cap
    return(px1)
  } else {
    stop("Check values")
  }
}




