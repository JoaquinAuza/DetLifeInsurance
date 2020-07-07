#' @title  Fractional Probability of Death

#' @description Calculates the fractional probability for a person of x+s/k dies before age x+(s+1)/k.
#' @param x An integer. The age of the insuree.
#' @param s An integer. Fraction of the year.
#' @param k An integer. Number of fractions per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame containing the mortality table, with the first column being the age and the second one, the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths and "constant" for constant force of mortality).
#' @keywords Fractional probability of death
#' @return The fractional probability of death.
#' @export
#' @examples
#' qfrac(27,1,4,0.04,CSO80MANB,"constant",1)
#' qfrac(20,0,12,0.04,CSO80MANB,"UDD",1)


qfrac<-function(x,s,k,i,data,assumption,prop){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(assumption=="UDD"){
    Q<-((1/k)*data[x+1,2]*prop)/(1-(s/k)*data[x+1,2]*prop)
  }else if(assumption=="constant"){
    ik<-Rate_converter(i,"i",1,"i",k,"frac")
    p<-((1+i)^(s/k))*(1-(s/k)*(1-E(x,1,i,data,prop,"none",1)))
    q.<-((1+i)^(s/k))*((1/k)*(1-E(x,1,i,data,prop,"none",1))*((s+1)*ik+1)-ik)
    Q<-q./p
  } else{
    stop("Check assumption")
  }
  return(as.numeric(Q))
}

