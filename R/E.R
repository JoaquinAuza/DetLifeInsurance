#' @title  Pure Endowment

#' @description Calculates the Pure endowments.
#' @param x An integer. The age of the insuree.
#' @param n The term of the endowment. An integer, for annual coverage, or a numeric for fractional coverage.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame containing the mortality table, with the first column being the age and the second one, the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param cap A numeric type value. The payment.
#' @references Chapter 2 of  Life Contingencies (1952) by Jordan.
#' @export
#' @keywords Pure Endowment
#' @return NULL
#' @examples
#' E(45,10,0.04,CSO80MANB,1,"none",1000)
#' E(24,1.6,0.04,CSO80MANB,1,"constant",17000)
#' E(26,2.4,0.04,CSO58FALB,1,"UDD",3500)
#'

E<-function(x,n,i=0.04,data,prop=1,assumption="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && n>=0 && i>=0 && prop>0 && cap>0){
    if(n==0){
      Exn<-1
    } else if(is_integer(n)==1){
      Exn<-(1/(1+i))^(n)*Survival(x,n,data,prop)
      if(is.na(Exn)==1){
        Exn<-0
      }
    } else {
      if(assumption=="constant"){
        t<-floor(n)
        sk<-n-t
        Exn<-E(x,t,i,data,prop,"none",1)-sk*(E(x,t,i,data,prop,"none",1)-E(x,t+1,i,data,prop,"none",1))
        if(is.na(Exn)==1){
          Exn<-0
        }
      }else{
        if(assumption=="UDD"){
          if((x+n)==(nrow(data)-1)){
            prop<-1
          }
          t<-floor(n)
          sk<-n-t
          prob<-1-sk*data[x+n+1,2]*prop
          Exn<-(1/(1+i))^(n)*(Survival(x,t,data,prop)*prob)
          if(is.na(Exn)==1){
            Exn<-0
          }
        }else{
          stop("Check assumption")
        }
      }
    }
    Exn<-as.numeric(Exn)
    px1<-Exn*cap
    return(px1)
  } else{
    stop("Check values")
  }
}




