#' @title  Reserve Valuation for Life Annuities

#' @description Calculates the reserve for the life Annuity up to the moment 't'.
#' @param px A numeric value. The value of the premium paid in each period.
#' @param x An integer. The age of the insuree.
#' @param h An integer. The deferral period.
#' @param n An integer. Number of years of coverage.
#' @param k An integer. Number of payments per year.
#' @param cantprem An integer. The total number of premiums.
#' @param premperyear An integer. The number of premiums to be paid per year.
#' @param i The interest rate. A numeric type value.
#' @param data A data.frame containing the mortality table, with the first column being the age and the second one, the probability of death.
#' @param prop A numeric value. It represents the proportion of the mortality table used (between 0 and 1).
#' @param assumption A character string. The assumption used for fractional ages ("UDD" for uniform distribution of deaths, "constant" for constant force of mortality and "none" if there is no fractional coverage).
#' @param cap A numeric type value. The annualized value of the payment.
#' @param t An integer. The moment of valuation (in months if it is a fractional coverage or in years if it is not).
#' @export
#' @references Chapter 5 of  Life Contingencies (1952) by Jordan, Chapter 11 of  Actuarial Mathematics for Life Contingent Risks (2009) by Dickson, Hardy and Waters.
#' @keywords Reserve Life Annuities
#' @return A data frame with Premium, Risk, 1/E and reserve values up to the moment t.
#' @examples
#' V_a(147.814202915034,20,5,10,1,5,1,0.04,CSO80MANB,1,"none",100,15)
#' V_a(148.324902023591/12,20,5,10,4,60,12,0.04,CSO80MANB,1,"constant",100,178)
#' V_a(223633.861110949,25,0,25,12,10,1,0.04,CSO80MANB,1,"UDD",120000,300)
#'

V_a<-function(px,x,h,n,k=1,cantprem=1,premperyear=1,i=0.04,data,prop=1,assumption="none",cap,t){
  dig<-getOption("digits")
  on.exit(options(digits = dig))
  options(digits = 15)
  reserve<-c()
  res<-0
  rown<-c()
  if(px>0 && x>=0 && is_integer(x)==1 && h>=0 && is_integer(h)==1 && n>0 && is_integer(n)==1 && k>=1 && is_integer(k)==1 && cantprem>=1 && is_integer(cantprem)==1 && premperyear>=1 && premperyear<=12 && is_integer(premperyear)==1 && i>=0 && prop>0 && cap>0){
    if(k==1 && premperyear==1){
      if(t<=(h+n)){
        reserve<-c()
        res<-0
        for(j in 1:t){
          risk<-0
          prem<-px
          if(j>h){
            risk<-cap
          }
          if(j>cantprem){
            prem<-0
          }
          res<-(res+prem-risk)*(E(x+j-1,1,i,data,prop,"none",1))^(-1)
          e<-(E(x+j-1,1,i,data,prop,"none",1))^(-1)
          reserve<-rbind(reserve,c(prem,risk,e,round(res,3)))
          rown<-c(rown,paste("Year",j))
        }
        colnames(reserve)<-c("Premium","Risk","1/E","Reserve")
        rownames(reserve)<-rown
      } else {
        stop("Check Year")
      }
    } else if(k<=12){
      if(t<=(h+n)*12){
        Premiums_Paid<-0
        frac<-1
        for(s in 1:t){
          risk<-0
          prem<-0
          age<-trunc((s-1)/12)
          if(s>h*12 & contmeses(s,k)==1){
            risk<-(cap/k)
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
    } else{
      stop("Check k")
    }
  }else{
    stop("Check values")
  }
  return(as.data.frame(reserve))
}





