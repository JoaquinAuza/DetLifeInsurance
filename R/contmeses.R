#' @title Contmeses
#'
#' @description Helpful function for identifying if a payment is required on that period.
#' @param x A numeric type value. The moment.
#' @param var A numeric type value. Number of payments per year.
#' @keywords Contmeses
#' @return Returns 1 if in that moment corresponds a payment or 0 if not.
#' @noRd
#'

contmeses<-function(x,var){
  j<-12/var
  if((x-1)/12==round((x-1)/12)){
    div<-1
  } else if(round((x-1)/j)==(x-1)/j){
    div<-1
  } else{
    div<-0
  }
  return(div)
}

