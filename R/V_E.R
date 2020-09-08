#' @title  Reserve Valuation for Pure Endowments

#' @description Calculates the reserve for the Pure endowments up to the moment t.
#' @param px A numeric value. The value of the premium paid in each period.
#' @param x An integer. The age of the insuree.
#' @param n The term of the endowment. An integer, for annual coverage, or a numeric for fractional coverage.
#' @param cantprem An integer. The total number of premiums.
#' @param premperyear An integer. The number of premiums to be paid per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame containing the mortality table, with the first column being the age and the second one, the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param cap A numeric type value. The payment.
#' @param t An integer. The moment of valuation (in months if it is a fractional coverage or in years if it is not).
#' @export
#' @keywords Reserve Pure Endowments
#' @references Chapter 5 of  Life Contingencies (1952) by Jordan, Chapter 11 of  Actuarial Mathematics for Life Contingent Risks (2009) by Dickson, Hardy and Waters.
#' @return A data frame with Premium, Risk, 1/E and reserve values up to the moment t.
#' @examples
#' V_E(663.501989747591,20,10,1,1,0.04,CSO80MANB,1,"none",1000,10)
#' V_E(9383.64446819386/12,20,2,12,12,0.04,CSO80MANB,1,"constant",10000,24)
#' V_E(9383.64446819386/12,20,2,12,12,0.04,CSO80MANB,1,"constant",10000,24)
#'


V_E<-function(px,x,n,cantprem=1,premperyear=1,i=0.04,data,prop=1,assumption="none",cap,t){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  reserve<-c()
  res<-0
  rown<-c()
  if(px>0 && x>=0 && is_integer(x)==1 && n>0 & cantprem>=1 && is_integer(cantprem)==1 && premperyear>=1 && premperyear<=12 && is_integer(premperyear)==1 && i>=0 && prop>0 && cap>0){
    if(is_integer(n)==1 & premperyear==1){
      if(t<=n){
        for(j in 1:(t+1)){
          risk<-0
          prem<-px
          if(j==(n+1)){
            risk<-cap
          }
          if(j>cantprem){
            prem<-0
          }
          res<-(res+prem-risk)*(E(x+j-1,1,i,data,prop,"none",1))^(-1)
          e<-(E(x+j-1,1,i,data,prop,"none",1))^(-1)
          reserve<-rbind(reserve,c(prem,risk,e,round(res,4)))
          rown<-c(rown,paste("Year",j))
        }
        rownames(reserve)<-rown
        colnames(reserve)<-c("Premium","Risk","1/E","Reserve")
      } else{
        stop("Check Year")
      }
    } else if(premperyear>=1) {
      if(t<=n*12){
        Premiums_Paid<-0
        frac<-1
        for(s in 1:(t+1)){
          risk<-0
          prem<-0
          age<-trunc((s-1)/12)
          if(s==(n*12+1)){
            risk<-cap
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
          }
        }
        colnames(reserve)<-c("Premium","Risk","1/E","Reserve")
        rownames(reserve)<-rown
      } else{
        stop("Check Month")
      }
    }else{
      stop("Check premperyear")
    }
  } else{
    stop("Check values")
  }
  return(as.data.frame(reserve))
}

