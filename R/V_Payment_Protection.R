#' @title   Reserve valuation for Payment Protection

#' @description Calculates the reserve for the loan insurance up to the moment t.
#' @param px A numeric value. The value of the premium paid in each period.
#' @param x An integer. The age of the insuree.
#' @param n An integer. Loan term (in years).
#' @param k An integer. Number of payments per year.
#' @param cantprem An integer. The total number of premiums.
#' @param premperyear An integer. The number of premiums to be paid per year.
#' @param i The interest rate. A numeric type value.
#' @param ip The interest rate of the loan. A numeric type value.
#' @param data A data.frame of the mortality table, with the first column being the age and the second one the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table used (between 0 and 1).
#' @param type A character string. The type of loan protection/reimburstment ("outstanding_debt" or "payments").
#' @param method A character string. Amortization scheme ("constant_instalment", "interest_only" or "constant_principal").
#' @param V0 A numeric type value. Loan value.
#' @param t An integer. The moment of valuation (in months if it is a fractional coverage or in years if it is not).
#' @export
#' @keywords Payment Protection
#' @return Returns the actuarial present value of the loan protection.
#' @examples
#' px1<-31.6216618772779
#' c1<-10500
#' V_Payment_Protection(px1,30,25,1,10,1,0.06,0.07,CSO80FANB,1,"payments","constant_instalment",c1,25)
#'


V_Payment_Protection<-function(px,x,n,k=1,cantprem=1,premperyear=1,i=0.04,ip=0.04,data,prop=1,type="outstanding_debt",method="interest_only",V0,t){
   dig<-getOption("digits")
   on.exit(options(digits = dig))
   options(digits = 15)
   reserve<-c()
   res<-0
   rown<-c()
  if(px>0 && is_integer(x)==1 && x>0 && is_integer(n)==1 && n>0 && is_integer(k)==1 && k>=1 && k<=12 && is_integer(cantprem)==1 && cantprem>=1 && is_integer(premperyear)==1 && premperyear>=1 && premperyear<=12 && i>0 && ip>0 && prop>0 && V0>0 && is_integer(t)==1){
 if(k==1 && premperyear==1){
 if(t<=n){
   v<-1/(1+i)
   if(method=="constant_instalment"){
   cf<-V0/af(1,n,ip)
     if(type=="outstanding_debt"){
     for(s in 1:t){
       risk<-(1-Survival(x+s-1,1,data,prop))*v*cf*af(0,n-s+1,ip)
       prem<-px
       if(s>cantprem){
         prem<-0
       }
       e<-E(x+s-1,1,i,data,prop)^(-1)
       res<-(res+prem-risk)*e
       reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
       rown<-rbind(rown,paste("Year",s))
     }
   }else if(type=="payments"){
     for(s in 1:t){
       risk<-(1-Survival(x+s-1,1,data,prop))*v*cf*af(0,n-s+1,i)
       prem<-px
       if(s>cantprem){
         prem<-0
       }
       e<-E(x+s-1,1,i,data,prop)^(-1)
       res<-(res+prem-risk)*e
       reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
       rown<-rbind(rown,paste("Year",s))
     }
   }else{
     stop("Check type")
   }
 } else if(method=="interest_only"){
   if(type=="outstanding_debt"){
     for(s in 1:t){
       risk<-(1-Survival(x+s-1,1,data,prop))*v*V0*(1+ip)
       prem<-px
       if(s>cantprem){
         prem<-0
       }
       e<-E(x+s-1,1,i,data,prop)^(-1)
       res<-(res+prem-risk)*e
       reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
       rown<-rbind(rown,paste("Year",s))
     }
   }else if(type=="payments"){
     for(s in 1:t){
       risk<-(1-Survival(x+s-1,1,data,prop))*v*(V0*v^(n-s)+V0*ip*af(0,n-s+1,i))
       prem<-px
       if(s>cantprem){
         prem<-0
       }
       e<-E(x+s-1,1,i,data,prop)^(-1)
       res<-(res+prem-risk)*e
       reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
       rown<-rbind(rown,paste("Year",s))
     }
   }else{
     stop("Check type")
   }
 } else if(method=="constant_principal"){
   if(type=="outstanding_debt"){
     for(s in 1:t){
       risk<-(1-Survival(x+s-1,1,data,prop))*v*V0*(1+ip)*((n-s+1)/n)
       prem<-px
       if(s>cantprem){
         prem<-0
       }
       e<-E(x+s-1,1,i,data,prop)^(-1)
       res<-(res+prem-risk)*e
       reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
       rown<-rbind(rown,paste("Year",s))
     }
   }else if(type=="payments"){
     for(s in 0:(t-1)){
       paym<-0
       for(j in 0:(n-s-1)){
       paym<-paym+(V0/n+((n-s-j)/n)*V0*ip)*v^(j)
       }
       risk<-(1-Survival(x+s,1,data,prop))*v*paym
       prem<-px
       if((s+1)>cantprem){
         prem<-0
       }
       e<-E(x+s,1,i,data,prop)^(-1)
       res<-(res+prem-risk)*e
       reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
       rown<-rbind(rown,paste("Year",s+1))
     }
   }else{
     stop("Check type")
   }
 }else{
   stop("Check Method")
 }
 }else{
   stop("Check t")
 }
 }else{
   if(t<=n*12){
   if(method=="constant_instalment"){
      ipk<-Rate_converter(ip,"i",1,"i",k,"frac")
      cf<-V0/af(1,n*k,ipk)
      prem_paid<-0
      deferral<-0
      j<--1
      Payment_Period<-12/k
      Elapsed_t_Payment<-0
      frac<-1
      v<-1/(1+i)
      if(type=="outstanding_debt"){
        for(s in 1:t){
         age<-trunc((s-1)/12)
         prem<-0
         if(contmeses(s,premperyear)==1 && cantprem>prem_paid){
            prem<-px
            prem_paid<-prem_paid+1
         }
         if(contmeses(s,k)==1){
            j<-j+1
         }
         risk<-qfrac(x+age,deferral,12,i,data,"UDD",prop)*v^((Payment_Period-Elapsed_t_Payment)/12)*cf*af(0,n*k-j,ipk)
         deferral<-deferral+1
         Elapsed_t_Payment<-Elapsed_t_Payment+1
         e<-E(x+age,(frac-1)/12,i,data,prop,"UDD",1)*(E(x+age,frac/12,i,data,prop,"UDD",1))^(-1)
         res<-(res+prem-risk)*e
         reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
         rown<-c(rown,paste("Month",s))
         frac<-frac+1
         if(round(s/12)==s/12){
            frac<-1
            deferral<-0
         }
         if(Elapsed_t_Payment==Payment_Period){
            Elapsed_t_Payment<-0
         }
        }
     }else if(type=="payments"){
        ik<-Rate_converter(i,"i",1,"i",k,"frac")
        for(s in 1:t){
           age<-trunc((s-1)/12)
           prem<-0
           if(contmeses(s,premperyear)==1 && cantprem>prem_paid){
              prem<-px
              prem_paid<-prem_paid+1
           }
           if(contmeses(s,k)==1){
              j<-j+1
           }
           risk<-qfrac(x+age,deferral,12,i,data,"UDD",prop)*v^((Payment_Period-Elapsed_t_Payment)/12)*cf*af(0,n*k-j,ik)
           deferral<-deferral+1
           Elapsed_t_Payment<-Elapsed_t_Payment+1
           e<-E(x+age,(frac-1)/12,i,data,prop,"UDD",1)*(E(x+age,frac/12,i,data,prop,"UDD",1))^(-1)
           res<-(res+prem-risk)*e
           reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
           rown<-c(rown,paste("Month",s))
           frac<-frac+1
           if(round(s/12)==s/12){
              frac<-1
              deferral<-0
           }
           if(Elapsed_t_Payment==Payment_Period){
              Elapsed_t_Payment<-0
           }
        }
     }else{
       stop("Check type")
     }
   } else if(method=="interest_only"){
      ipk<-Rate_converter(ip,"i",1,"i",k,"frac")
      prem_paid<-0
      deferral<-0
      Payment_Period<-12/k
      Elapsed_t_Payment<-0
      frac<-1
      v<-1/(1+i)
     if(type=="outstanding_debt"){
        for(s in 1:t){
           age<-trunc((s-1)/12)
           prem<-0
           if(contmeses(s,premperyear)==1 && cantprem>prem_paid){
              prem<-px
              prem_paid<-prem_paid+1
           }
           risk<-qfrac(x+age,deferral,12,i,data,"UDD",prop)*v^((Payment_Period-Elapsed_t_Payment)/12)*V0*(1+ipk)
           deferral<-deferral+1
           Elapsed_t_Payment<-Elapsed_t_Payment+1
           e<-E(x+age,(frac-1)/12,i,data,prop,"UDD",1)*(E(x+age,frac/12,i,data,prop,"UDD",1))^(-1)
           res<-(res+prem-risk)*e
           reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
           rown<-c(rown,paste("Month",s))
           frac<-frac+1
           if(round(s/12)==s/12){
              frac<-1
              deferral<-0
           }
           if(Elapsed_t_Payment==Payment_Period){
              Elapsed_t_Payment<-0
           }
        }
     }else if(type=="payments"){
        j<--1
        ik<-Rate_converter(i,"i",1,"i",k,"frac")
        for(s in 1:t){
           age<-trunc((s-1)/12)
           prem<-0
           if(contmeses(s,premperyear)==1 && cantprem>prem_paid){
              prem<-px
              prem_paid<-prem_paid+1
           }
           if(contmeses(s,k)==1){
              j<-j+1
           }
           risk<-qfrac(x+age,deferral,12,i,data,"UDD",prop)*(V0*v^(n-s/12+1/12)+V0*ipk*af(0,n*k-j,ik)*v^((Payment_Period-Elapsed_t_Payment)/12))
           deferral<-deferral+1
           Elapsed_t_Payment<-Elapsed_t_Payment+1
           e<-E(x+age,(frac-1)/12,i,data,prop,"UDD",1)*(E(x+age,frac/12,i,data,prop,"UDD",1))^(-1)
           res<-(res+prem-risk)*e
           reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
           rown<-c(rown,paste("Month",s))
           frac<-frac+1
           if(round(s/12)==s/12){
              frac<-1
              deferral<-0
           }
           if(Elapsed_t_Payment==Payment_Period){
              Elapsed_t_Payment<-0
           }
        }
     }else{
       stop("Check type")
     }
   } else if(method=="constant_principal"){
      ipk<-Rate_converter(ip,"i",1,"i",k,"frac")
      prem_paid<-0
      deferral<-0
      j<--1
      Payment_Period<-12/k
      Elapsed_t_Payment<-0
      frac<-1
      v<-1/(1+i)
      if(type=="outstanding_debt"){
         for(s in 1:t){
            age<-trunc((s-1)/12)
            prem<-0
            if(contmeses(s,premperyear)==1 && cantprem>prem_paid){
               prem<-px
               prem_paid<-prem_paid+1
            }
            if(contmeses(s,k)==1){
               j<-j+1
            }
            risk<-qfrac(x+age,deferral,12,i,data,"UDD",prop)*v^((Payment_Period-Elapsed_t_Payment)/12)*(V0*(1+ipk)*(n*k-j)/(n*k))
            deferral<-deferral+1
            Elapsed_t_Payment<-Elapsed_t_Payment+1
            e<-E(x+age,(frac-1)/12,i,data,prop,"UDD",1)*(E(x+age,frac/12,i,data,prop,"UDD",1))^(-1)
            res<-(res+prem-risk)*e
            reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
            rown<-c(rown,paste("Month",s))
            frac<-frac+1
            if(round(s/12)==s/12){
               frac<-1
               deferral<-0
            }
            if(Elapsed_t_Payment==Payment_Period){
               Elapsed_t_Payment<-0
            }
         }
     }else if(type=="payments"){
        ik<-Rate_converter(i,"i",1,"i",k,"frac")
        vk<-1/(1+ik)
        for(s in 1:t){
           age<-trunc((s-1)/12)
           prem<-0
           if(contmeses(s,premperyear)==1 && cantprem>prem_paid){
              prem<-px
              prem_paid<-prem_paid+1
           }
           if(contmeses(s,k)==1){
              j<-j+1
           }
           paym<-0
           for(a in 0:(n*k-j-1)){
              paym<-paym+(V0/(n*k))*(1+(n*k-j-a)*ipk)*vk^(a)
           }
           risk<-qfrac(x+age,deferral,12,i,data,"UDD",prop)*v^((Payment_Period-Elapsed_t_Payment)/12)*paym
           deferral<-deferral+1
           Elapsed_t_Payment<-Elapsed_t_Payment+1
           e<-E(x+age,(frac-1)/12,i,data,prop,"UDD",1)*(E(x+age,frac/12,i,data,prop,"UDD",1))^(-1)
           res<-(res+prem-risk)*e
           reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
           rown<-c(rown,paste("Month",s))
           frac<-frac+1
           if(round(s/12)==s/12){
              frac<-1
              deferral<-0
           }
           if(Elapsed_t_Payment==Payment_Period){
              Elapsed_t_Payment<-0
           }
        }
     }else{
       stop("Check type")
     }
   }else{
     stop("Check Method")
   }
   }else{
     stop("Check t")
   }
 }
   } else{
   stop("Check values")
   }
  colnames(reserve)<-c("Premium","Risk","1/E","Reserve")
  rownames(reserve)<-rown
  return(as.data.frame(reserve))
}

