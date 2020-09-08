#' @title Varying Life Insurance: Arithmetic Progression

#' @description Calculates the present value of a varying life insurance according to a arithmetic progression.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Fractions per year.
#' @param r The variation rate. A numeric type value.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param variation A character string. "inter" if the variation it's interannual or "intra" if it's intra-annual.
#' @param cap A numeric type value. Amount insured for the first year/period.
#' @export
#' @keywords Life Insurance Arithmetic Progression
#' @return Returns a numeric value (actuarial present value).
#' @references Chapter 4 of  Actuarial Mathematics for Life Contingent Risks (2009) by Dickson, Hardy and Waters.
#' @note For an increasing life insurance coverage, 'r' must be 1.
#' @examples
#' Av.(43,0,4,1,0.7,0.04,CSO80MANB,1,"none","none",1)
#' Av.(37,1,6,1,0.3,0.04,CSO80MANB,1,"none","none",1)
#' Av.(25,2,3,2,0.6,0.04,CSO80MANB,1,"constant","inter",1)
#' Av.(37,3,6,4,0.5,0.04,CSO80MANB,1,"constant","intra",1)
#' Av.(40,3,5,2,0.4,0.04,CSO80MANB,1,"UDD","inter",1)
#' Av.(50,2,4,4,0.6,0.04,CSO80MANB,1,"UDD","intra",1)
#'


Av.<-function(x,h,n,k=1,r=1,i=0.04,data,prop=1,assumption="none",variation="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && i>=0 && prop>0 && cap>0){
    if(n==0){
      Avxhn<-0
    } else if(k==1){
      Avxhn<-0
      p<-Survival(x,h,data,prop)
      for(s in h:(h+n-1)){
        if(x+s==(nrow(data)-1)){
          prop<-1
        }
        Avxhn<-Avxhn+(1/(1+i))^(s+1)*(as.numeric(data[x+s+1,2])*prop)*p*(1+r*(s-h))
        p<-p*(1-data[x+s+1,2]*prop)
        if(x+s==nrow(data)-1){
          break
        }
      }
    } else{
      if(assumption=="constant"){
        if(variation=="inter"){
          fk<-Rate_converter(i,"i",1,"f",k,"frac")
          Avxhn<-E(x,h,i,data,prop,"none",1)+r*a(x,h+1,n-1,1,i,data,prop,"none",1)-(1+(n-1)*r)*E(x,h+n,i,data,prop,"none",1)-fk*av(x,h,n,k,r,i,data,prop,"constant","inter",1)
        } else if(variation=="intra"){
          Avxhn<-0
          v<-(1/(1+i))
          ik<-Rate_converter(i,"i",1,"i",k,"frac")
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              q<-((1+i)^(s/k))*((1/k)*(1-E(x+t,1,i,data,prop,"none",1))*((s+1)*ik+1)-ik)
              Avxhn<-Avxhn+(1+(t-h)*k*r+s*r)*E(x,t,i,data,prop,"none",1)*q*v^((s+1)/k)
            }
            if(x+t==nrow(data)-1){
              break
            }
          }
        } else {
          stop("Check variation")
        }
      } else if(assumption=="UDD"){
        if(variation=="inter"){
          jk<-Rate_converter(i,"i",1,"j",k,"frac")
          Avxhn<-(i/jk)*Av.(x,h,n,1,r,i,data,prop,"none","none",1)
        } else if(variation=="intra"){
          Avxhn<-0
          v<-(1/(1+i))
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              if(x+t==(nrow(data)-1)){
                prop<-1
              }
              q<-((1/k)*data[x+t+1,2])*prop
              Avxhn<-Avxhn+(1+(t-h)*k*r+s*r)*E(x,t,i,data,prop,"none",1)*q*v^((s+1)/k)
            }
            if(x+t==nrow(data)-1){
              break
            }
          }
        } else {
          stop("Check variation")
        }
      } else{
        stop("Check assumption")
      }
    }
    Avxhn<-as.numeric(Avxhn)
    px1<-Avxhn*cap
    return(px1)
  } else {
    stop("Chack values")
  }
}


