#' @title  Payment Protection

#' @description Calculates the present value of the loan insurance.
#' @param x An integer. The age of the insuree.
#' @param n An integer. Loan term (in years).
#' @param k An integer. Number of payments per year.
#' @param V0 A numeric type value. Loan value.
#' @param i The interest rate. A numeric type value.
#' @param ip The interest rate of the loan. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table being used (between 0 and 1).
#' @param type A character string. The type of loan protection/reimburstment ("outstanding_debt" or "payments").
#' @param method A character string. Amortization scheme ("constant_instalment", "interest_only" or "constant_principal").
#' @export
#' @keywords Payment Protection
#' @return Returns a numeric value (actuarial present value).
#' @examples
#' Payment_Protection(35,2,1,1000000,0.04,0.06,CSO80MANB,1,"payments","constant_instalment")
#' Payment_Protection(43,2,1,1000000,0.04,0.07,CSO80MANB,1,"outstanding_debt","constant_instalment")
#' Payment_Protection(30,2,2,1000000,0.04,0.06,CSO80MANB,1,"payments","constant_instalment")
#' Payment_Protection(20,2,2,1000000,0.04,0.07,CSO80MANB,1,"outstanding_debt","constant_instalment")
#' Payment_Protection(33,2,1,1000000,0.04,0.05,CSO80MANB,1,"payments","interest_only")
#' Payment_Protection(56,2,1,1000000,0.04,0.06,CSO80MANB,1,"outstanding_debt","interest_only")
#' Payment_Protection(40,2,2,1000000,0.04,0.06,CSO80MANB,1,"payments","interest_only")
#' Payment_Protection(25,2,2,1000000,0.04,0.05,CSO80MANB,1,"outstanding_debt","interest_only")
#' Payment_Protection(23,2,1,1000000,0.04,0.07,CSO80MANB,1,"payments","constant_principal")
#' Payment_Protection(35,2,1,1000000,0.04,0.06,CSO80MANB,1,"outstanding_debt","constant_principal")
#' Payment_Protection(45,2,2,1000000,0.04,0.05,CSO80MANB,1,"payments","constant_principal")
#' Payment_Protection(35,2,2,1000000,0.04,0.07,CSO80MANB,1,"outstanding_debt","constant_principal")
#'
#'


Payment_Protection<-function(x,n,k=1,V0,i=0.04,ip=0.04,data,prop=1,type="outstanding_debt",method="interest_only"){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  if(x>=0 && is_integer(x)==1 && n>=0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && V0>0 && i>=0 && ip>=0 && prop>0 && x+n<=(nrow(data)-1) && x+n<=nrow(data)-1){
    ipk<-Rate_converter(ip,"i",1,"i",k,"frac")
    if(type=="outstanding_debt"){
      if(method=="constant_instalment"){
        c<-V0/af(1,n*k,ipk)
        sd<-0
        v<-1/(1+i)
        p<-1
        for(s in 0:(n-1)){
          for(z in 0:(k-1)){
            if(x+s==(nrow(data)-1)){
              prop<-1
            }
            sd<-sd+(as.numeric(data[x+s+1,2]))*prop*p*(1/k)*v^(s+(z+1)/k)*c*af(0,n*k-s*k-z,ipk)
          }
          p<-p*(1-data[x+s+1,2]*prop)
        }
        return(as.numeric(sd))
      } else if(method=="interest_only"){
        sd<-(V0*(1+ipk))*A.(x,0,n,k,i,data,prop,"UDD",1)
        return(as.numeric(sd))
      } else if(method=="constant_principal") {
        sd<-0
        v<-1/(1+i)
        p<-1
        for(s in 0:(n-1)){
          for(z in 0:(k-1)){
            if(x+s==(nrow(data)-1)){
              prop<-1
            }
            sd<-sd+(as.numeric(data[x+s+1,2])*prop)*p*(1/k)*v^(s+(z+1)/k)*(V0*(1+ipk)*((n*k-s*k-z)/(n*k)))
          }
          p<-p*(1-data[x+s+1,2]*prop)
        }
        return(as.numeric(sd))
      } else{
        stop("Check method")
      }
    } else if(type=="payments"){
      if(method=="constant_instalment"){
        c<-V0/af(1,n*k,ipk)
        v<-1/(1+i)
        dk<-Rate_converter(i,"i",1,"d",k,"frac")
        q<-0
        p<-1
        for(s in 0:(n-1)){
          if(x+s==(nrow(data)-1)){
            prop<-1
          }
          q<-q+as.numeric(data[x+s+1,2]*prop)*p
          p<-p*(1-data[x+s+1,2]*prop)
        }
        sc<-(c/dk)*(A.(x,0,n,k,i,data,prop,"UDD",1)-v^(n+1/k)*q)
        return(as.numeric(sc))
      } else if(method=="interest_only"){
        v<-1/(1+i)
        dk<-Rate_converter(i,"i",1,"d",k,"frac")
        q<-0
        p<-1
        for(s in 0:(n-1)){
          if(x+s==(nrow(data)-1)){
            prop<-1
          }
          q<-q+as.numeric(data[x+s+1,2]*prop)*p
          p<-p*(1-data[x+s+1,2]*prop)
        }
        sc<-V0*v^(n)*q+((V0*ipk)/dk)*(A.(x,0,n,k,i,data,prop,"UDD",1)-v^(n+1/k)*q)
        return(as.numeric(sc))
      } else if(method=="constant_principal") {
        c<-V0/af(1,n*k,ipk)
        sc<-0
        v<-1/(1+i)
        vk<-(v)^(1/k)
        p<-1
        for(s in 0:(n-1)){
          for(z in 0:(k-1)){
            upperlimit<-n*k-s*k-z
            presval<-0
            for(j in 0:(upperlimit-1)){
              presval<-presval+((V0/(n*k))+((n*k-k*s-z-j)/(n*k))*V0*ipk)*vk^(j)
            }
            if(x+s==(nrow(data)-1)){
              prop<-1
            }
            sc<-sc+(as.numeric(data[x+s+1,2])*prop)*p*(1/k)*v^(s+(z+1)/k)*presval
          }
          p<-p*(1-data[x+s+1,2]*prop)
        }
        return(as.numeric(sc))
      } else{
        stop("Check method")
      }
    } else{
      stop("Check type")
    }
  } else{
    stop("Check values")
  }
}

