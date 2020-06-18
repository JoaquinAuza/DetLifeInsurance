#' @title  Loan Amortization

#' @description Calculates the amortization schedule.
#' @param V0  A numeric type value. Loan value.
#' @param n A numeric type value. The number of payments.
#' @param i A numeric type value or a vector of them. The interest rate of the loan.
#' @param i2 A numeric type value. The interest rate of the saving account.
#' @param alic A numeric type value. Interest tax rate.
#' @param ins A numeric type value. The rate of V0 to be paid in each period.
#' @param method A string. Amortization method used ("constant_installment","interest_only", "constant_principal", "interest_only_wsavings_account" or "constant_installment_varintrate" ).
#' @return Returns a data.frame object containing Period, Payment, Pure Payment, Intrest, Amortization, Insurance, TAX and Outstanding debt.
#' @export
#' @keywords Loan Amortization
#' @return NULL
#' @examples
#' Loan_amortization(1000,12,0.04,0,0.21,0.01,"constant_installment")
#' Loan_amortization(12000,15,0.04,0,0.21,0.01,"interest_only")
#' Loan_amortization(13000,10,0.04,0,0.21,0.01,"constant_principal")
#' Loan_amortization(15000,20,0.04,0.05,0.21,0.01,"interest_only_wsavings_account")
#' Loan_amortization(5000,5,0.04,0,0.21,0.01,"constant_installment_varintrate")
#'
#'

Loan_amortization<-function(V0,n,i,i2=0,alic=0,ins=0,method){
  options(digitis = 15)
  if(V0>0 && n>0 && is_integer(n)==1 && i>=0  && i2>=0 && alic>=0 && ins>=0){
    if(method=="constant_installment"){
      c<-V0/af(1,n,i)
      Period<-c(0,1:n)
      Prev_outstanding_debt<-V0
      Int<-c(0)
      Amort<-c(0)
      Outstanding_debt<-c(V0)
      TAX<-c(0)
      Pure_Payment<-c(0,rep(c,n))
      Payment<-c(0)
      Insurance<-c(0)
      for(j in 1:n){
        I<-Prev_outstanding_debt*i
        tax<-I*alic
        Int<-c(Int,I)
        TAX<-c(TAX,tax)
        insurance<-Prev_outstanding_debt*ins
        Insurance<-c(Insurance,insurance)
        amort<-c-I
        Amort<-c(Amort,amort)
        Prev_outstanding_debt<-Prev_outstanding_debt-amort
        Outstanding_debt<-c(Outstanding_debt,Prev_outstanding_debt)
        cc<-c+tax+insurance
        Payment<-c(Payment,cc)
      }
      loan.info<-data.frame(Period,Payment,Pure_Payment,Int,Amort,TAX,Insurance,Outstanding_debt)
      return(loan.info)
    }else if(method=="interest_only"){
      Period<-c(0,1:n)
      int<-V0*i
      tax<-int*alic
      TAX<-c(0,rep(tax,n))
      Int<-c(0,rep(int,n))
      insurance<-V0*ins
      Insurance<-c(0,rep(insurance,n))
      Payment<-c(0,rep(int+tax+insurance,(n-1)),(int+tax+insurance+V0))
      Amort<-c(rep(0,n),V0)
      Outstanding_debt<-c(rep(V0,n),(V0-(Payment[n+1]-(int+tax+insurance))))
      loan.info<-data.frame(Period,Payment,Int,TAX,Amort,Insurance,Outstanding_debt)
      return(loan.info)
    }else if(method=="constant_principal"){
      amort<-V0/n
      Period<-c(0,1:n)
      Amort<-c(0,rep(amort,n))
      Payment<-c(0)
      Pure_Payment<-c(0)
      Int<-c(0)
      Outstanding_debt<-c(V0)
      Prev_outstanding_debt<-V0
      TAX<-c(0)
      Insurance<-c(0)
      for(j in 1:n){
        cc<-amort+(V0-amort*(j-1))*i
        int<-(V0-amort*(j-1))*i
        tax<-int*alic
        TAX<-c(TAX,tax)
        Int<-c(Int,int)
        insurance<-Prev_outstanding_debt*ins
        Insurance<-c(Insurance,insurance)
        Prev_outstanding_debt<-Prev_outstanding_debt-amort
        Outstanding_debt<-c(Outstanding_debt,Prev_outstanding_debt)
        payment=cc+tax+insurance
        Payment<-c(Payment,payment)
        Pure_Payment<-c(Pure_Payment,cc)
      }
      loan.info<-data.frame(Period,Payment,Pure_Payment,Int,Amort,TAX,Insurance,Outstanding_debt)
      return(loan.info)
    }else if(method=="interest_only_wsavings_account"){
      cah<-V0/sf(0,n,i2)
      Period<-c(0,1:n)
      int<-V0*i
      tax<-int*alic
      TAX<-c(0,rep(tax,n))
      Int<-c(0,rep(int,n))
      insurance<-V0*ins
      Insurance<-c(0,rep(insurance,n))
      Payment<-c(0,rep(int+tax+insurance,n))
      Amort<-c(rep(0,n),V0)
      Outstanding_debt<-rep(V0,n)
      Real_outstanding_debt<-c(0)
      Outstanding_debt_with_savings <-(0)
      for(j in 1:20){
        Savings<-cah*sf(0,j,i2)
        real<-Outstanding_debt[j]-Savings
        Real_outstanding_debt<-c(Real_outstanding_debt,real)
        Outstanding_debt_with_savings <-c(Outstanding_debt_with_savings,Savings)
      }
      Outstanding_debt<-c(Outstanding_debt,(V0-Outstanding_debt_with_savings [n+1]-(int+tax+insurance))+Payment[n+1])
      loan.info<-data.frame(Period,Payment,Int,TAX,Amort,Insurance,Outstanding_debt,Real_outstanding_debt,Outstanding_debt_with_savings)
      return(loan.info)
    }else if(method=="constant_installment_varintrate"){
      irate<-c(i,rep(i[length(i)],(n-length(i))))
      Period<-c(0,1:n)
      Prev_outstanding_debt<-V0
      c=V0*((1+i[1])^(n)*(irate[1]))/((1+i[1])^(n)-1)
      Pure_Payment<-c(0,c)
      int<-Prev_outstanding_debt*irate[1]
      Int<-c(0,int)
      tax<-int*alic
      TAX<-c(0,tax)
      amort<-c-int
      Amort<-c(0,amort)
      insurance<-Prev_outstanding_debt*ins
      Prev_outstanding_debt<-V0-amort
      Outstanding_debt<-c(V0,Prev_outstanding_debt)
      Insurance<-c(0,insurance)
      Rate<-c(0,i)
      Payment<-c(0,c+tax+insurance)
      for(j in 2:n){
        if(irate[j]==irate[j-1]){
          c=c
        }else{
          c=Prev_outstanding_debt*((1+irate[j])^(n-(j-1))*(irate[j]))/((1+irate[j])^(n-(j-1))-1)
        }
        Pure_Payment<-c(Pure_Payment,c)
        I<-Prev_outstanding_debt*(irate[j])
        tax<-I*alic
        Int<-c(Int,I)
        insurance<-Prev_outstanding_debt*ins
        Insurance<-c(Insurance,insurance)
        cc<-c+tax+insurance
        Payment<-c(Payment,cc)
        TAX<-c(TAX,tax)
        amort<-c-I
        Amort<-c(Amort,amort)
        Prev_outstanding_debt<-Prev_outstanding_debt-amort
        Outstanding_debt<-c(Outstanding_debt,Prev_outstanding_debt)
      }
      loan.info<-data.frame(Period,Rate,Payment,Pure_Payment,Int,Amort,Insurance,TAX,Outstanding_debt)
      return(loan.info)
    } else{
      stop("Check method")
    }
  }else{
    stop("Check values")
  }
}


