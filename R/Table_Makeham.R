#' @title  Makeham's Law of Mortality Table Creator

#' @description Creates a mortality table under Makeham's law.
#' @param x0  A numeric type value. The initial age of the table.
#' @param omega A numeric type value. The final age of the table.
#' @param A A numeric type value. A parameter of the law.
#' @param B A numeric type value. A parameter of the law.
#' @param C A numeric type value. A parameter of the law.
#' @return Returns a data.frame object containing age and death probabilities.
#' @export
#' @keywords Makeham's law table.
#' @references Chapter 3 (p 77-78) of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @note The parameters are usually confined to the ranges shown below:
#'  0.001 < A < 0.003, 10^(-6) < B < 10(-3), 1.08 < C < 1.12.
#' @examples
#' Table_Makeham(0,100,0.002,3*10^(-4),1.124)
#'


Table_Makeham<-function(x0,omega,A,B,C){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x0>=0 && omega>=0 && A>(-B) && 0<B  && C>1 && is_integer(x0)==1 && is_integer(omega)==1){
    S<-exp(-A)
    G<-exp(-B/(log(C)))
    if(x0>=omega){
      stop("Check Omega")
    } else{
      X<-seq(0,omega-1)
      Q<-rep(NA,omega)
      Tabla.Makeham<-cbind(X,Q)
      for(i in x0:(omega-1)){
        P<-S*G^(C^(i)*(C-1))
        q<-1-P
        Tabla.Makeham[i+1,2]<-q
      }
      Tabla.Makeham[omega,2]<-1
      colnames(Tabla.Makeham)<-c("x","q")
      Tabla.Makeham<-as.data.frame(Tabla.Makeham)
      return(Tabla.Makeham)
    }
  } else {
    stop("Check values")
  }
}

