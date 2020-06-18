#' @title  Dormoy's Law of Mortality Table Creator

#' @description Creates a mortality table under Dormoy's law.
#' @param x0  A numeric type value. The initial age of the table.
#' @param omega A numeric type value. The final age of the table.
#' @param a A numeric type value. A parameter of the law.
#' @return Returns a data.frame object containing age and death probabilities.
#' @export
#' @keywords Dormoy's law mortality table.
#' @references Chapter 3 (p 77-78) of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' Table_Dormoy(0,100,0.98)
#'

Table_Dormoy<-function(x0,omega,a){
  options(digits = 15)
  if(x0>=0 && omega>=0 && a>0 && a<1 && is_integer(x0)==1 && is_integer(omega)==1){
    if(x0>=omega){
      stop("Check Omega")
    } else{
      X<-seq(0,omega-1)
      Q<-rep(NA,omega)
      Tabla.Dormoy<-cbind(X,Q)
      for(i in x0:(omega-2)){
        q<-1-(a^1)
        Tabla.Dormoy[i+1,2]<-q
      }
      Tabla.Dormoy[omega,2]<-1
      colnames(Tabla.Dormoy)<-c("x","q")
      Tabla.Dormoy<-as.data.frame(Tabla.Dormoy)
      return(Tabla.Dormoy)
    }
  } else {
    stop("Check values")
  }
}


