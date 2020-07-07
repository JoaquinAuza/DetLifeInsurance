#' @title  de Moivre's Law of Mortality Table Creator

#' @description Creates a mortality table under de Moivre's law.
#' @param x0  A numeric type value. The initial age of the table.
#' @param omega A numeric type value. The final age of the table.
#' @return Returns a data.frame object containing age and death probabilities.
#' @export
#' @keywords de Moivre's law table.
#' @references  Chapter 3 (p 77-78) of  Actuarial Mathematics (1997) by Bowers, Gerber, Hickman, Jones & Nesbitt.
#' @examples
#' Table_Moivre(0,100)
#'

Table_Moivre<-function(x0,omega){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x0>=0 && omega>=0 && is_integer(x0)==1 && is_integer(omega)==1){
    if(x0>=omega){
      stop("Check Omega")
    } else{
      X<-seq(0,omega-1)
      Q<-rep(NA,omega)
      Tabla.Moivre<-cbind(X,Q)
      for(i in x0:(omega-1)){
        q<-1/(omega-i)
        Tabla.Moivre[i+1,2]<-q
      }
      colnames(Tabla.Moivre)<-c("x","q")
      Tabla.Moivre<-as.data.frame(Tabla.Moivre)
      return(Tabla.Moivre)
    }
  } else {
    stop("Check values")
  }
}


