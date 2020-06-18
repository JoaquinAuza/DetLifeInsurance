#' @title  Gompertz's Law of Mortality Table Creator

#' @description Creates a mortality table under Gompertz's law.
#' @param x0  A numeric type value. The initial age of the table.
#' @param omega A numeric type value. The final age of the table.
#' @param B A numeric type value. A parameter of the law.
#' @param C A numeric type value. A parameter of the law.
#' @return Returns a data.frame object containing age and death probabilities.
#' @export
#' @keywords Gompertz's law table.
#' @references Chapter 3 (p 77-78) of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' Table_Gompertz(0,100,0.00008,1.07)
#'

Table_Gompertz<-function(x0,omega,B,C){
  options(digits = 15)
  if(x0>=0 && omega>=0 && 0<B  && C>1 && is_integer(x0)==1 && is_integer(omega)==1) {
    G<-exp(-B/(log(C)))
    if(x0>=omega){
      stop("Check Omega")
    } else {
      X<-seq(0,omega-1)
      Q<-rep(NA,omega)
      Tabla.Gompertz<-cbind(X,Q)
      for(i in x0:(omega-1)){
        q<-1-(G^(C^(i)*(C-1)))
        Tabla.Gompertz[i+1,2]<-q
      }
    }
    Tabla.Gompertz[omega,2]<-1
    colnames(Tabla.Gompertz)<-c("x","q")
    Tabla.Gompertz<-as.data.frame(Tabla.Gompertz)
    return(Tabla.Gompertz)
  } else{
    stop("Check values")
  }
}


