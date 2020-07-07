#' @title  Fractional table of mortality

#' @description Creates a fractional mortality table for a given mortality table.
#' @param data A data.frame of the annual mortality table, with the first column being the age and the second one the probability of death.
#' @param frac An integer. The number of fractions per year.
#' @param i A numeric type value. The interest rate.
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths and "constant" for constant force of mortality).
#' @return Returns a data.frame object containing fractional age and death probability vectors.
#' @export
#' @keywords Fractional Table of Mortality.
#' @references Chapter 3 of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt
#' @examples
#' Fractional_table(CSO80MANB,2,0.04,"constant")
#' Fractional_table(CSO80MANB,2,0.04,"UDD")
#'
Fractional_table <-function(data,frac,i=0.04,assumption="UDD"){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  X<-c()
  Q<-c()
  if(assumption=="UDD" & frac>0 & is_integer(frac)==1){
    for(x in 1:nrow(data)){
      for(k in 0:(frac-1)){
        q<-((1/frac)*data[x,2])/(1-(k/frac)*data[x,2])
        age<-x+(k/frac)-1
        X<-c(X,age)
        Q<-c(Q,q)
      }
    }
  } else if(assumption=="constant" & frac>0 & is_integer(frac)==1 & i>0){
    for(x in 1:nrow(data)){
      for(k in 0:(frac-1)){
        ik<-Rate_converter(i,"i",1,"i",frac,"frac")
        p<-((1+i)^(k/frac))*(1-(k/frac)*(1-E(x-1,1,i,data,1,"none",1)))
        q<-((1+i)^(k/frac))*((1/frac)*(1-E(x-1,1,i,data,1,"none",1))*((k+1)*ik+1)-ik)
        q.<-q/p
        age<-x+(k/frac)-1
        X<-c(X,age)
        Q<-c(Q,q.)
      }
    }
  } else{
    stop("Check assumption, frac and i")
  }
  rown<-seq(1,nrow(data)*frac)
  Table.Frac<-as.data.frame(cbind(X,Q))
  rownames(Table.Frac)<-rown
  return(Table.Frac)
}
