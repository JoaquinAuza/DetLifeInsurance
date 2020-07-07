#' @title Varying Life Annuities: Arithmetic Progression

#' @description Calculates the present value of a varying life annuity according to a arithmetic progression.
#' @param x An integer. The age on the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of payments per year.
#' @param r The variation rate. A numeric type value.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param variation A character string. "inter" if the variation it's interannual or "intra" if it's intra-annual.
#' @param cap A numeric type value. The annualized value of the first payment.
#' @export
#' @keywords Life Annuities Arithmetic Progression
#' @return Returns a numeric value (actuarial present value).
#' @references Chapter 5 of  Actuarial Mathematics for Life Contingent Risks (2009) by Dickson, Hardy and Waters.
#' @note For an increasing life annuity coverage, 'r' must be 1.
#' @examples
#' av(33,0,5,1,0.8,0.04,CSO80MANB,1,"none","none",1)
#' av(26,2,4,1,0.4,0.04,CSO80MANB,1,"none","none",1)
#' av(26,1,5,4,0.5,0.04,CSO80MANB,1,"constant","inter",1)
#' av(24,1,3,3,0.7,0.04,CSO80MANB,1,"constant","intra",1)
#' av(35,4,6,6,0.4,0.04,CSO80MANB,1,"UDD","inter",1)
#' av(40,3,7,2,0.7,0.04,CSO80MANB,1,"UDD","intra",1)
#'

av<-function(x,h,n,k=1,r=1,i=0.04,data,prop=1,assumption="none",variation="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && i>=0 && prop>0 && cap>0){
    if(n==0){
      avxhn<-0
    } else if(k==1){
      avxhn<-0
      for(j in h:(h+n-1)){
        avxhn<-avxhn+E(x,j,i,data,prop,"none",1)*(1+r*(j-h))
      }
    }else{
      if(assumption=="constant"){
        if(variation=="inter"){
          avxhn<-(1-((k-1)/(2*k)))*av(x,h,n,1,r,i,data,prop,"none","none",1)+((k-1)/(2*k))*av(x,h+1,n,1,r,i,data,prop,"none","none",1)
        } else if(variation=="intra"){
          avxhn<-0
          for(t in h:(n+h-1)){
            for(s in 0:(k-1)){
              avxhn<-avxhn+(1+(t-h)*k*r+s*r)*(1/k)*E(x,t,i,data,prop,"none",1)*E(x+t,s/k,i,data,prop,"constant",1)
            }
          }
        }else{
          stop("Check variation")
        }
      } else if(assumption=="UDD"){
        if(variation=="inter"){
          fk<-Rate_converter(i,"i",1,"f",k,"frac")
          avxhn<-(E(x,h,i,data,prop,"none",1)+r*a(x,h+1,n-1,1,i,data,prop,"none",1)-(1+(n-1)*r)*E(x,h+n,i,data,prop,"none",1)-Av.(x,h,n,k,r,i,data,prop,"UDD","inter",1))/fk
        }else if(variation=="intra"){
          avxhn<-0
          for(t in h:(n+h-1)){
            for(s in 0:(k-1)){
              avxhn<-avxhn+(1+(t-h)*k*r+s*r)*(1/k)*E(x,t,i,data,prop,"none",1)*E(x+t,s/k,i,data,prop,"UDD",1)
            }
          }
        }else {
          stop("Check variation")
        }
      } else{
        stop("Check assumption")
      }
    }
    avxhn<-as.numeric(avxhn)
    px1<-avxhn*cap
    return(px1)
  } else{
    stop("Check values")
  }
}



