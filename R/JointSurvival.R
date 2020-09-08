#' @title  Joint Survival Probability

#' @description Calculates the probability of survival given a mortality table for a group.
#' @param x A vector representing the age of each individual.
#' @param n An integer. The term.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one, the probability of death.
#' @param prop A numeric value. The proportion of the mortality table used, between 0 and 1.
#' @export
#' @keywords Joint Survival Probability
#' @return NULL
#' @examples
#' ages<-c(34,45,52,65)
#' JointSurvival(ages,10,CSO80FALB)
#'

JointSurvival<-function(x,n,data,prop=1){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  aux_prop<-prop
  if(is_integer(x)==1 && is_integer(n)==1 && n>=0 && prop>0 ){
    TotalProb<-1
    for(i in 1:length(x)){
      Prob<-1
      if(n==0){
        Prob<-1
      }else{
        for(l in x[i]:(x[i]+n-1)){
          if(l==nrow(data)-1){
            prop<-1
          } else {
            prop<-aux_prop
          }
          Prob<-Prob*(1-data[l+1,2]*prop)
          if(is.na(Prob)==1){
            prob<-0
          }
        }
        Prob<-as.numeric(Prob)
      }
      TotalProb<-TotalProb*Prob
    }
    return(TotalProb)
  } else{
    stop("Check values")
  }
}


