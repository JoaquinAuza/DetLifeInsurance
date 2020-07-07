#' @title Fractional Premium
#'
#' @description Calculates the annualized value of the fractional premiums.
#' @param px1 A numeric type value. The value of the single net premium.
#' @param x An integer. The age of the insuree.
#' @param m An integer. Years of premium payment.
#' @param k An integer. Number of premiums per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table used (between 0 and 1).
#' @param effect A character string. This parameter indicates if, in the event of death, the insuree is released from paying the remaining fractional premiums of that year ("yes" or "no")
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths and "constant" for constant force of mortality).
#' @export
#' @keywords Fractional Premium
#' @return Returns the annualized value of the fractional premium.
#' @references Chapter 4 of  Actuarial Mathematics for Life Contingent Risks (2009) by Dickson, Hardy and Waters
#' @note If k=1, regardless of the "effect", the returned value is the annual premium.
#' @examples
#' PremiumFrac(1000,20,10,2,0.04,CSO80MANB,1,"yes","constant")
#' PremiumFrac(1000,20,10,2,0.04,CSO80MANB,1,"no","UDD")
#'


PremiumFrac<-function(px1,x,m,k,i=0.04,data,prop=1,effect="yes",assumption){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(k==1){
    premium<-px1/a(x,0,m,k,i,data,prop,assumption,1)
    return(premium)
  }else{
    if(effect=="yes"){
      Annualpremium<-px1/a(x,0,m,k,i,data,prop,assumption,1)
      return(Annualpremium)
    }else{
      if(effect=="no" && k>1 && k<=12){
        Annualpremiumnofrac<-px1/a(x,0,m,1,i,data,prop,assumption,1)
        v<-1/(1+i)
        d<-Rate_converter(i,"i",1,"d",1,"frac")
        fk<-Rate_converter(i,"i",1,"f",k,"frac")
        Annualpremium<-Annualpremiumnofrac*(fk/d)
        return(Annualpremium)
      }else{
        stop("Check effect or value of k")
      }
    }
  }
}
