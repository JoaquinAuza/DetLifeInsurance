#' @title  Reserve Valuation for Decreasing Life Insurance

#' @description Calculates the reserve for the decreasing life insurance up to the moment t.
#' @param px A numeric value. The value of the premium paid in each period.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of fractions per year.
#' @param cantprem An integer. The total number of premiums.
#' @param premperyear An integer. The number of premiums to be paid per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame containing the mortality table, with the first column being the age and the second one, the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param variation A character string. "inter" if the variation it's inter-annual or "intra" if it's intra-annual.
#' @param cap A numeric type value. Amount insured for the first year/period.
#' @param t An integer. The moment of valuation (in months if it is a fractional coverage or in years if it is not).
#' @export
#' @references Chapter 5 of  Life Contingencies (1952) by Jordan, Chapter 11 of  Actuarial Mathematics for Life Contingent Risks (2009) by Dickson, Hardy and Waters.
#' @keywords Reserve Varying Life Annuities Decreasing
#' @return A data frame with Premium, Risk, 1/E and reserve values up to the moment t.
#' @examples
#' V_AD.(251.489227521537,20,2,2,1,2,1,0.04,CSO80MANB,1,"none","none",100000,4)
#' V_AD.(432.974179723949/12,20,2,2,2,24,12,0.04,CSO80MANB,1,"UDD","intra",100000,48)
#' V_AD.(258.794207318685/12,20,2,2,2,24,12,0.04,CSO80MANB,1,"UDD","inter",100000,48)
#' V_AD.(412.784641829906/12,20,2,2,2,24,12,0.04,CSO80MANB,1,"constant","intra",100000,48)
#' V_AD.(258.189935788232/12,20,2,2,2,24,12,0.04,CSO80MANB,1,"constant","inter",100000,48)


V_AD.<-function(px,x,h,n,k=1,cantprem=1,premperyear=1,i=0.04,data,prop=1,assumption="none",variation="none",cap,t){
  options(digits = 15)
  reserve<-c()
  res<-0
  rown<-c()
  if(px>0 && x>=0 && is.integer(x)==1 && h>=0 && is.integer(h)==1 && n>0 && is.integer(n)==1 && k>=1 && is.integer(k)==1 && cantprem>=1 && is.integer(cantprem)==1 && premperyear>=1 && premperyear<=12 && is.integer(premperyear)==1 && i>=0 && prop>0 && cap>0){
    if(k==1 && premperyear==1){
      if(t<=(h+n)){
        for(j in 1:t){
          risk<-0
          prem<-px
          if(j>h){
            risk<-A.(x+j-1,0,1,1,i,data,prop,"none",1)*cap*(n-(j-h-1))
          }
          if(j>cantprem){
            prem<-0
          }
          res<-(res+prem-risk)*(E(x+j-1,1,i,data,prop,"none",1))^(-1)
          e<-(E(x+j-1,1,i,data,prop,"none",1))^(-1)
          reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
          rown<-c(rown,paste("Month",j))
        }
        colnames(reserve)<-c("Premium","Risk","E","Reserve")
        rownames(reserve)<-rown
      } else{
        stop("Check Period")
      }
    }else if(k<=12){
      if(t<=(n+h)*12){
        Premiums_Paid<-0
        frac<-1
        v<-1/(1+i)
        Payment_Period<-12/k
        Elapsed_t_Payment<-0
        deferral<-0
        CumVariationIntra<-n*k+1
        CumVariationInter<-n
        InterCont<-0
        if(variation=="inter"){
          for(s in 1:t){
            risk<-0
            prem<-0
            age<-trunc((s-1)/12)
            if(s>h*12 ){
              InterCont<-InterCont+1
              risk<-(cap)*(CumVariationInter)*qfrac(x+age,deferral,12,i,data,assumption,prop)*v^((Payment_Period-Elapsed_t_Payment)/12)
              deferral<-deferral+1
              Elapsed_t_Payment<-Elapsed_t_Payment+1
              if(InterCont==12){
                CumVariationInter<-CumVariationInter-1
                InterCont<-0
              }
            }
            if(contmeses(s,premperyear)==1 & Premiums_Paid<cantprem){
              prem<-px
              Premiums_Paid<-Premiums_Paid+1
            }
            va<-(res+prem-risk)*E(x+age,(frac-1)/12,i,data,prop,assumption,1)
            res<-va*(E(x+age,frac/12,i,data,prop,assumption,1))^(-1)
            e<-E(x+age,(frac-1)/12,i,data,prop,assumption,1)*(E(x+age,frac/12,i,data,prop,assumption,1))^(-1)
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
          colnames(reserve)<-c("Premium","Risk","E","Reserve")
          rownames(reserve)<-rown
        }else if(variation=="intra"){
          for(s in 1:t){
            risk<-0
            prem<-0
            age<-trunc((s-1)/12)
            if(s>h*12 ){
              if(contmeses(s,k)==1){
                CumVariationIntra<-CumVariationIntra-1
              }
              risk<-(cap)*CumVariationIntra*qfrac(x+age,deferral,12,i,data,assumption,prop)*v^((Payment_Period-Elapsed_t_Payment)/12)
              deferral<-deferral+1
              Elapsed_t_Payment<-Elapsed_t_Payment+1
            }
            if(contmeses(s,premperyear)==1 & Premiums_Paid<cantprem){
              prem<-px
              Premiums_Paid<-Premiums_Paid+1
            }
            va<-(res+prem-risk)*E(x+age,(frac-1)/12,i,data,prop,assumption,1)
            res<-va*(E(x+age,frac/12,i,data,prop,assumption,1))^(-1)
            e<-E(x+age,(frac-1)/12,i,data,prop,assumption,1)*(E(x+age,frac/12,i,data,prop,assumption,1))^(-1)
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
          colnames(reserve)<-c("Premium","Risk","E","Reserve")
          rownames(reserve)<-rown
        } else{
          stop("Check variation")
        }
      }else{
        stop("Check Month")
      }
    }else{
      stop("Check k")
    }
  }else{
    stop("Check values")
  }
  return(as.data.frame(reserve))
}

