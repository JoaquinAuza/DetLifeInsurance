#' @title Decreasing Life Insurance

#' @description Calculates the present value of a decreasing life insurance.
#' @param x  An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Fractions per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table  being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param variation A character string. "inter" if the variation it's interannual or "intra" if it's intra-annual.
#' @param cap A numeric type value. Amount insured for the first year/period.
#' @export
#' @keywords Life Insurance Decreasing
#' @return Returns a numeric  value (actuarial present value).
#' @references Chapter 3 of  Life Contingencies (1952) by Jordan, chapter 4 of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' AD.(56,0,8,1,0.04,CSO80MANB,1,"none","none",1)
#' AD.(39,1,10,1,0.04,CSO80MANB,1,"none","none",1)
#' AD.(37,6,11,4,0.04,CSO80MANB,1,"constant","inter",1)
#' AD.(21,2,5,4,0.04,CSO80MANB,1,"UDD","inter",1)
#' AD.(54,4,16,2,0.04,CSO80MANB,1,"constant","intra",1)
#' AD.(20,10,15,3,0.04,CSO80MANB,1,"UDD","intra",1)
#'
AD.<-function(x,h,n,k=1,i=0.04,data,prop=1,assumption="none",variation="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && i>=0 && prop>0 && cap>0){
    if(n==0){
      ADxhn<-0
    } else  if(k==1){
      ADxhn<-0
      p<-Survival(x,h,data,prop)
      for(s in h:(h+n-1)){
        ADxhn<-ADxhn+(1/(1+i))^(s+1)*(as.numeric(data[x+s+1,2])*prop)*p*(n-s+h)
        p<-p*(1-data[x+s+1,2]*prop)
      }
    } else {
      if(assumption=="constant") {
        if(variation=="inter") {
          ADxhn<-0
          v<-1/(1+i)
          ik<-Rate_converter(i,"i",1,"i",k,"frac")
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              q<-((1+i)^(s/k))*((1/k)*(1-E(x+t,1,i,data,1,"none",1))*((s+1)*ik+1)-ik)
              ADxhn<-ADxhn+(n-(t-h))*E(x,t,i,data,prop,"none",1)*q*v^((s+1)/k)
            }
          }
        }else if(variation=="intra"){
          ADxhn<-0
          v<-1/(1+i)
          ik<-Rate_converter(i,"i",1,"i",k,"frac")
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              q<-((1+i)^(s/k))*((1/k)*(1-E(x+t,1,i,data,1,"none",1))*((s+1)*ik+1)-ik)
              ADxhn<-ADxhn+(n*k-(t-h)*k-s)*E(x,t,i,data,prop,"none",1)*q*v^((s+1)/k)
            }
          }
        } else{
          stop("Check variation")
        }
      } else if(assumption=="UDD") {
        if(variation=="inter"){
          ADxhn<-0
          v<-1/(1+i)
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              q<-((1/k)*data[x+t+1,2])
              ADxhn<-ADxhn+(n-(t-h))*E(x,t,i,data,prop,"none",1)*q*v^((s+1)/k)
            }
          }
        }else if(variation=="intra") {
          ADxhn<-0
          v<-1/(1+i)
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              q<-((1/k)*data[x+t+1,2])
              ADxhn<-ADxhn+(n*k-(t-h)*k-s)*E(x,t,i,data,prop,"none",1)*q*v^((s+1)/k)
            }
          }

        }else {
          stop("Check variation")
        }
      } else {
        stop("Check assumption")
      }
    }
    ADxhn<-as.numeric(ADxhn)
    px1<-ADxhn*cap
    return(px1)
  }else {
    stop("Check values")
  }
}

