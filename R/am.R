#' @title  Life Annuities for a group

#' @description Calculates the present value of a life annuity for a group.
#' @param x A vector of intergers representing the age of each individual of the group.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of payments per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age, and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param type A character string. Conditions to be met in order to access the benefit of the annuity ("joint", "exactly" or "atleast").
#' @param quant An integer. Required only if type is not "joint". If type is "exactly" it represents the exact amount of survivors required for the endowment to be payed. If type is "atleast", it represents the minimum number of survivors required.
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param cap A numeric type value. The annualized value of the payment.
#' @export
#' @keywords Group Life Annuities
#' @return Returns a numeric value (actuarial present value).
#' @examples
#' ages<-c(23,34,21)
#' ages<-c(23,34,21)
#' am(ages,5,10,2,0.05,CSO80MALB,1,"joint",assumption="UDD")
#' am(ages,0,20,1,0.06,CSO80FALBsmoker,1,"atleast",1)
#' am(ages,2,15,2,0.07,CSO80FANBsmoker,0.8,"exactly",2,"constant")
#'

am<-function(x,h,n,k=1,i=0.04,data,prop=1,type="joint",quant=1,assumption="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(is_integer(x)==1 && is_integer(h)==1 && h>=0 && is_integer(n)==1 && n>=0 && is_integer(k)==1 && i>=0 && prop>0){
    if(n==0){
      amxhn<-0
    }else if(type=="joint"){
      if(assumption=="none"){
        amxhn<-0
        for(j in h:(h+n-1)){
          amxhn<-amxhn+Em(x,j,i,data,prop,"joint",quant,"none",1)
        }
        if(is.na(amxhn)==1){
          amxhn<-0
        }
      } else if(assumption=="constant"){
        amxhn<-am(x,h,n,1,i,data,prop,"joint")-((k-1)/(2*k))*(Em(x,h,i,data,prop,"joint")-Em(x,h+n,i,data,prop,"joint"))
        if(is.na(amxhn)==1){
          amxhn<-0
        }
      }else if(assumption=="UDD"){
        amxhn<-0
        for(j in h:(h+n-1)){
          for(s in 0:(k-1)){
            amxhn<-amxhn+(1/k)*Em(x,j+s/k,i,data,prop,"joint",quant,assumption="UDD",1)
          }
        }
      }else{
        stop("Check assumption")
      }
    }else if(type=="exactly" && is_integer(quant)==1 && quant>0 && quant<=length(x)){
        amxhn<-0
        for(g1 in quant:length(x)){
          amxhn_partial<-0
          coeff<-ncol(combn(g1,(g1-quant)))*(-1)^(g1-quant)
          combinations<-combn(x,g1)
          for(g2 in 1:ncol(combinations)){
            amxhn_partial<-amxhn_partial+am(combinations[,g2],h,n,k,i,data,prop,type="joint",quant,assumption,1)
          }
          amxhn<-amxhn+amxhn_partial*coeff
        }
        if(is.na(amxhn)==1){
          amxhn<-0
        }
    }else if(type=="atleast" && is_integer(quant)==1 && quant>0 && quant<=length(x)){
        amxhn<-0
        for(g1 in quant:length(x)){
          amxhn_partial<-0
          coeff<-ncol(combn(g1-1,(g1-quant)))*(-1)^(g1-quant)
          combinations<-combn(x,g1)
          for(g2 in 1:ncol(combinations)){
            amxhn_partial<-amxhn_partial+am(combinations[,g2],h,n,k,i,data,prop,type="joint",quant,assumption,1)
          }
          amxhn<-amxhn+amxhn_partial*coeff
        }
        if(is.na(amxhn)==1){
          amxhn<-0
        }
    } else{
      stop("Check type")
    }
    amxhn<-as.numeric(amxhn)
    px1<-amxhn*cap
    return(px1)
  } else{
    stop("Check values")
  }
}
