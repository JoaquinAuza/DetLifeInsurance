#' @title Life Insurance of a group

#' @description Calculates the present value of a life insurance coverage for a group.
#' @param x A vector of intergers representing the age of each individual of the group.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of fractions per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param ndeath An integer. Number of deaths necessary for payment to occur.
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param cap A numeric type value. The value of the payment.
#' @export
#' @keywords Group Life Insurance
#' @return Returns a numeric value (actuarial present value).
#' @examples
#' ages<-c(22,33,44,55,66)
#' Am.(ages,5,15,1,0.04,CSO80MANB,1,2,"none",1)
#' Am.(ages,0,20,4,0.04,CSO80MANB,1,2,"UDD",1)
#' Am.(ages,10,25,2,0.04,CSO80MANB,1,2,"constant",1)
#'


Am.<-function(x,h,n,k=1,i=0.04,data,prop=1,ndeath=1,assumption="none",cap=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>=0 && is_integer(n)==1 && is_integer(k)==1 && k>=1 && k<=12 &&i>=0 && prop>0 && is_integer(ndeath)==1 && ndeath<=length(x) && ndeath>0){
    if(n==0){
      Amxhn<-0
    }else if(k==1){
      quantity<-length(x)-ndeath+1
      d<-Rate_converter(i,"i",1,"d",1,"frac")
      Amxhn<-Em(x,h,i,data,prop,type="atleast",quant=quantity,assumption="none",1)-Em(x,h+n,i,data,prop,type="atleast",quant=quantity,assumption="none",1)-d*am(x,h,n,k,i,data,prop,type="atleast",quant=quantity,assumption="none",1)
    }else if(assumption=="UDD" || assumption=="constant" && k>1){
      quantity<-length(x)-ndeath+1
      v<-1/(1+i)
      fk<-d<-Rate_converter(i,"i",1,"f",k,"frac")
      Amxhn<-Em(x,h,i,data,prop,type="atleast",quant=quantity)-Em(x,h+n,i,data,prop,type="atleast",quant=quantity)-fk*am(x,h,n,k,i,data,prop,type="atleast",quant=quantity,assumption,1)
    }else{
      stop("Check assumption")
    }
    Amxhn<-as.numeric(Amxhn)
    px1<-Amxhn*cap
    return(px1)
  }else{
    stop("Check values")
  }
}



