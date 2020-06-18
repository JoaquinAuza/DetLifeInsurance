#' @title  Survival Probability

#' @description Calculates the probability of survival given a mortality table for an individual or a group.
#' @param x An integer or a vector including only integers representing the age of each individual.
#' @param n An integer. The term.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one, the probability of death.
#' @param prop A numeric value. The proportion of the mortality table used, between 0 and 1.
#' @export
#' @keywords Survival Probability
#' @return NULL
#' @examples
#' Survival(20,2,CSO58MANB,1)
#'


Survival<-function(x,n,data,prop=1){
  options(digits = 15)
  if(x>=0 & is_integer(x)==1 & n>=0 & is_integer(n)==1 & prop>0){
    Prob<-1
    if(n==0){
      Prob<-1
    }else{
      for(l in x:(x+n-1)){
        Prob<-Prob*(1-data[l+1,2]*prop)
      }
      Prob<-as.numeric(Prob)
    }
    return(Prob)
  } else{
    stop("Check values")
  }
}


