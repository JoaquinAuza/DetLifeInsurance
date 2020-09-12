#' @title  Group Pure Endowment

#' @description Calculates the Pure endowments for a group of insurees.
#' @param x A vector of integers. The age of the insurees.
#' @param n The term of the endowment. An integer, for annual coverage, or a numeric for fractional coverage.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame containing the mortality table, with the first column being the age and the second one, the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param type A character string. Conditions to be met in order to access the benefit of the endowment ("joint", "exactly" or "atleast").
#' @param quant An integer. Required only if type is not "joint". If type is "exactly" it represents the exact amount of survivors required for the endowment to be payed. If type is "atleast", it represents the minimum number of survivors required.
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param cap A numeric type value. The payment.
#' @export
#' @keywords Pure Endowment
#' @return NULL
#' @examples
#' ages<-c(23,33,33)
#' Em(ages,15,0.04,CSO80MANB,1,"joint")
#' Em(ages,20.5,0.04,CSO80MANB,1,"joint",assumption = "constant",cap= 1)
#' Em(ages,10.5,0.04,CSO80MANB,1,"joint",assumption = "UDD", cap=1)
#' ages<-c(20,23,24,25)
#' Em(ages,15,0.04,CSO80MANB,1,"exactly",1,"none",1)
#' Em(ages,24.2,0.04,CSO80MANB,1,"exactly",2,"constant",1)
#' Em(ages,8.2,0.04,CSO80MANB,1,"exactly",3,"UDD",1)
#'
#' ages<-c(40,42,56,57,58,59)
#' Em(ages,15,0.04,CSO80MANB,1,"atleast",1,"none",1)
#' Em(ages,25.5,0.04,CSO80MANB,1,"atleast",4,"constant",1)
#' Em(ages,15.3,0.04,CSO80MANB,1,"atleast",3,"UDD",1)
#'
#'

Em<-function(x,n,i=0.04,data,prop=1,type="joint",quant=1,assumption="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(is_integer(x)==1 && n>=0 && i>=0 && prop>0){
    if(n==0){
      Emxn<-1
    }else if(type=="joint"){
      if(is_integer(n)==1){
        Emxn<-(1/(1+i))^(n)*JointSurvival(x,n,data,prop)
        if(is.na(Emxn)==1){
          Emxn<-0
        }
      } else {
        if(assumption=="constant"){
          t<-floor(n)
          sk<-n-t
          Emxn<-Em(x,t,i,data,prop,type)-sk*(Em(x,t,i,data,prop,type)-Em(x,t+1,i,data,prop,type))
          if(is.na(Emxn)==1){
            Emxn<-0
          }
        }else{
          if(assumption=="UDD"){
            t<-floor(n)
            sk<-n-t
            prob<-1-sk*(1-JointSurvival(x+t,1,data,prop))
            Emxn<-(1/(1+i))^(n)*(JointSurvival(x,t,data,prop)*prob)
            if(is.na(Emxn)==1){
              Emxn<-0
            }
          }else{
            stop("Check assumption")
          }
        }
      }
    }else if(type=="exactly" && is_integer(quant)==1 && quant>0 && quant<=length(x)){
      if(is_integer(n)==1){
        Emxn<-0
        for(g1 in quant:length(x)){
          Emxn_partial<-0
          coeff<-ncol(utils::combn(g1,(g1-quant)))*(-1)^(g1-quant)
          combinations<-utils::combn(x,g1)
          for(g2 in 1:ncol(combinations)){
            Emxn_partial<-Emxn_partial+Em(combinations[,g2],n,i,data,prop,type="joint",quant,"none",1)
          }
          Emxn<-Emxn+Emxn_partial*coeff
        }
        if(is.na(Emxn)==1){
          Emxn<-0
        }
      } else {
        if(assumption=="constant"){
          t<-floor(n)
          sk<-n-t
          Emxn<-Em(x,t,i,data,prop,type,quant)-sk*(Em(x,t,i,data,prop,type,quant)-Em(x,t+1,i,data,prop,type,quant))
          if(is.na(Emxn)==1){
            Emxn<-0
          }
        }else{
          if(assumption=="UDD"){
            t<-floor(n)
            sk<-n-t
            prob_integer<-0
            prob_sum_frac<-0
            for(g1 in quant:length(x)){
              prob_sum_partial<-0
              prob_sum_partial_frac<-0
              coeff<-ncol(utils::combn(g1,(g1-quant)))*(-1)^(g1-quant)
              combinations<-utils::combn(x,g1)
              for(g2 in 1:ncol(combinations)){
                prob_sum_partial<-prob_sum_partial+JointSurvival(combinations[,g2],t,data,prop)
                prob_sum_partial_frac<-prob_sum_partial+JointSurvival(combinations[,g2]+t,1,data,prop)
              }
              prob_integer<-prob_integer+prob_sum_partial*coeff
            }
            prob_frac<-1-sk*(1-prob_sum_frac)
            Emxn<-(1/(1+i))^(n)*(prob_integer*prob_frac)
            if(is.na(Emxn)==1){
              Emxn<-0
            }
          }else{
            stop("Check assumption")
          }
        }
      }
    } else if(type=="atleast" && is_integer(quant)==1 && quant>0 && quant<=length(x)){
      if(is_integer(n)==1){
        Emxn<-0
        for(g1 in quant:length(x)){
          Emxn_partial<-0
          coeff<-ncol(utils::combn(g1-1,(g1-quant)))*(-1)^(g1-quant)
          combinations<-utils::combn(x,g1)
          for(g2 in 1:ncol(combinations)){
            Emxn_partial<-Emxn_partial+Em(combinations[,g2],n,i,data,prop,type="joint",quant,"none",1)
          }
          Emxn<-Emxn+Emxn_partial*coeff
        }

        if(is.na(Emxn)==1){
          Emxn<-0
        }
      } else {
        if(assumption=="constant"){
          t<-floor(n)
          sk<-n-t
          Emxn<-Em(x,t,i,data,prop,type,quant)-sk*(Em(x,t,i,data,prop,type,quant)-Em(x,t+1,i,data,prop,type,quant))
          if(is.na(Emxn)==1){
            Emxn<-0
          }
        }else{
          if(assumption=="UDD"){
            t<-floor(n)
            sk<-n-t
            prob_integer<-0
            prob_sum_frac<-0
            for(g1 in quant:length(x)){
              prob_sum_partial<-0
              prob_sum_partial_frac<-0
              coeff<-ncol(utils::combn(g1-1,(g1-quant)))*(-1)^(g1-quant)
              combinations<-utils::combn(x,g1)
              for(g2 in 1:ncol(combinations)){
                prob_sum_partial<-prob_sum_partial+JointSurvival(combinations[,g2],t,data,prop)
                prob_sum_partial_frac<-prob_sum_partial+JointSurvival(combinations[,g2]+t,1,data,prop)
              }
              prob_integer<-prob_integer+prob_sum_partial*coeff
            }
            prob_frac<-1-sk*(1-prob_sum_frac)
            Emxn<-(1/(1+i))^(n)*(prob_integer*prob_frac)
            if(is.na(Emxn)==1){
              Emxn<-0
            }
          }else{
            stop("Check assumption")
          }
        }
      }
    }
    Emxn<-as.numeric(Emxn)
    px1<-Emxn*cap
    return(px1)
    } else{
    stop("Check values")
  }
}


