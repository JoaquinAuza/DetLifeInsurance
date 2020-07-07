#' @title Varying Life Annuities: Geometric Progression

#' @description Calculates the present value of a varying life annuity according to a geometric progression.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of payments per year.
#' @param r The variation rate. A numeric type value.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param variation A character string. "inter" if the variation it's interannual or "intra" if it's intra-annual.
#' @param cap A numeric type value. The annualized value of the  first payment.
#' @export
#' @keywords Life Annuities Geometric Progression
#' @return Returns a numeric value (actuarial present value).
#' @references Chapter 5 of  Actuarial Mathematics for Life Contingent Risks (2009) by Dickson, Hardy and Waters.
#' @examples
#' avg(33,0,5,1,0.8,0.04,CSO80MANB,1,"none","none",1)
#' avg(26,2,4,1,0.4,0.04,CSO80MANB,1,"none","none",1)
#' avg(20,2,2,2,0.15,0.04,CSO80MANB,1,"constant","inter",1)
#' avg(40,5,5,3,0.07,0.04,CSO80MANB,1,"constant","intra",1)
#' avg(27,0,15,4,0.06,0.04,CSO80MANB,1,"UDD","inter",1)
#' avg(34,7,12,6,0.03,0.04,CSO80MANB,1,"UDD","intra",1)
#'



avg<-function(x,h,n,k=1,r,i=0.04,data,prop=1,assumption="none",variation="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && i>=0 && prop>0 && cap>0){
    if(n==0){
      avgxhn<-0
    } else if(k==1){
      avgxhn<-0
      for(j in h:(h+n-1)){
        avgxhn<-avgxhn+E(x,j,i,data,prop,"none",1)*(1+r)^(j-h)
      }
    } else{
      if(assumption=="constant"){
        if(variation=="inter"){
          avgxhn<-0
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              avgxhn<-avgxhn+(1+r)^(t-h)*E(x,t,i,data,prop,"none",1)*E(x+t,s/k,i,data,prop,"constant",1)*(1/k)
            }
          }
        }else if(variation=="intra"){
          avgxhn<-0
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              avgxhn<-avgxhn+(1+r)^((t-h)*k+s)*E(x,t,i,data,prop,assumption,1)*E(x+t,s/k,i,data,prop,"constant",1)*(1/k)
            }
          }
        } else{
          stop("Check variation")
        }
      } else if(assumption=="UDD"){
        if(variation=="inter"){
          avgxhn<-0
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              avgxhn<-avgxhn+(1+r)^(t-h)*E(x,t,i,data,prop,assumption,1)*E(x+t,s/k,i,data,prop,"UDD",1)*(1/k)
            }
          }
        } else if(variation=="intra"){
          avgxhn<-0
          for(t in h:(h+n-1)){
            for(s in 0:(k-1)){
              avgxhn<-avgxhn+(1+r)^((t-h)*k+s)*E(x,t,i,data,prop,assumption,1)*E(x+t,s/k,i,data,prop,"UDD",1)*(1/k)
            }
          }
        }else{
          stop("Check variation")
        }
      }
    }
    avgxhn<-as.numeric(avgxhn)
    px1<-avgxhn*cap
    return(px1)
  } else {
    stop("Check values")
  }
}




