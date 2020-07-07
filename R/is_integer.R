#' @title  Integer

#' @description Returns 1 if the number or elements of a vector is integer or 0 if it's not.
#' @param x  A numeric type value or a vector.
#' @keywords Integer
#' @return NULL
#' @noRd
#'


is_integer<-function(x){
  suma<-0
  for(i in 1:length(x)){
    if(round(x[i],0)==x[i]){
      suma<-suma+1
    }else{
      suma<-suma+0
    }
  }
  if(suma==length(x)){
    return(1)
  }else{
    return(0)
  }
}


