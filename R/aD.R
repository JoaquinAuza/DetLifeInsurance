#' @title Decreasing Life Annuities

#' @description Calculates the present value of a decreasing life annuity.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of payments per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param variation A character string. "inter" if the variation it's interannual or "intra" if it's intra-annual.
#' @param cap A numeric type value. The annualized value of the first payment.
#' @export
#' @keywords Life Annuities Decreasing
#' @return Returns a numeric value (actuarial present value).
#' @references Chapter 2 of  Life Contingencies (1952) by Jordan, chapter 5 of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' aD(27,0,3,1,0.04,CSO80MANB,1,"none","none",1)
#' aD(32,2,8,1,0.04,CSO80MANB,1,"none","none",1)
#' aD(35,8,15,4,0.04,CSO80MANB,1,"constant","inter",1)
#' aD(21,2,5,4,0.04,CSO80MANB,1,"UDD","inter",1)
#' aD(54,4,16,2,0.04,CSO80MANB,1,"constant","intra",1)
#' aD(20,10,15,3,0.04,CSO80MANB,1,"UDD","intra",1)
#'

aD<-function(x,h,n,k=1,i=0.04,data,prop=1,assumption="none",variation="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && i>=0 && prop>0 && cap>0){
    if(n==0){
      adxhn<-0
    } else if(k==1){
      adxhn<-0
      for(j in h:(h+n-1)){
        adxhn<-adxhn+E(x,j,i,data,prop,"none",1)*(n-j+h)
      }
    } else {
      if(assumption=="constant"){
        if(variation=="inter"){
          adxhn<-0
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              adxhn<-adxhn+(n-(t-h))*E(x,t,i,data,prop,"none",1)*(1/k)*E(x+t,s/k,i,data,prop,"constant",1)
            }
          }
        } else if(variation=="intra"){
          adxhn<-0
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              adxhn<-adxhn+(n*k-(t-h)*k-s)*E(x,t,i,data,prop,"none",1)*(1/k)*E(x+t,s/k,i,data,prop,"constant",1)
            }
          }
        } else{
          stop("Check variation")
        }
      } else if(assumption=="UDD"){
        if(variation=="inter"){
          adxhn<-0
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              adxhn<-adxhn+(n-(t-h))*E(x,t,i,data,prop,"none",1)*(1/k)*E(x+t,s/k,i,data,prop,"UDD",1)
            }
          }
        }else if(variation=="intra"){
          adxhn<-0
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              adxhn<-adxhn+(n*k-(t-h)*k-s)*E(x,t,i,data,prop,"none",1)*(1/k)*E(x+t,s/k,i,data,prop,"UDD",1)
            }
          }
        } else{
          stop("Check variation")
        }
      } else{
        stop("Check assumption")
      }
    }
    adxhn<-as.numeric(adxhn)
    px1<-adxhn*cap
    return(px1)
  } else {
    stop("Check values")
  }
}



