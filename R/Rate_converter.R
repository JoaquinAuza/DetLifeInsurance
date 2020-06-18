#' @title  Interest & Discount Rate Converter

#' @description Converts nominal and effective interest and discount rates.
#' @param num  A numeric type value. It is the interest/discount rate to be converted.
#' @param rate1 A string ("i", "d","f" or "j"). Type of interest/discount rate to be converted.
#' @param m number of capitalizations.
#' @param rate2 A string ("i" for effective interest rate, "d" for effective discount rate,"f" for nominal discount rate, "j" for nominal interest rate).Type of interest/discount rate to obtain.
#' @param k An integer. Number of capitalizations per year.
#' @param type A string. Reference for "k", indicating whether it is expressed as a fraction or as days ("frac" or "days").
#' @export
#' @keywords Interest Rate Discount Rate
#' @return NULL
#' @examples
#' Rate_converter(0.04,"i",1,"i",6,"frac")
#' Rate_converter(0.04,"f",1,"j",6,"frac")
#' Rate_converter(0.04,"f",365,"d",60,"days")
#' Rate_converter(0.04,"f",365,"f",60,"days")
#'

Rate_converter<-function(num,rate1,m,rate2,k,type="days"){
  options(digits = 15)
  rate<-0
  if(type=="days"){
  if(rate1=="i"){
      if(rate2=="i"){
        rate<-((1+num)^(k/m))-1
      } else if(rate2=="d"){
          ik<-((1+num)^(k/m))-1
          rate<-ik/(1+ik)
      } else if(rate2=="j"){
        ik<-((1+num)^(k/m))-1
        rate<-(ik*365)/k
      }else if(rate2=="f"){
        ik<-((1+num)^(k/m))-1
        dk<-ik/(1+ik)
        rate<-(dk*365)/k
      } else{
        stop("Check rate2")
      }
  }else if(rate1=="d"){
    if(rate2=="i"){
      dk<-1-(1-num)^(k/m)
      rate<-dk/(1-dk)
    } else if(rate2=="d"){
      rate<-1-(1-num)^(k/m)
    } else if(rate2=="j"){
      dk<-1-(1-num)^(k/m)
      ik<-dk/(1-dk)
      rate<-(ik*365)/k
    }else if(rate2=="f"){
      dk<-1-(1-num)^(k/m)
      rate<-(dk*365)/k
    } else{
      stop("Check rate2")
    }
  }else if(rate1=="j"){
    if(rate2=="i"){
      im<-(num*m)/365
      rate<-(1+im)^(k/m)-1
    } else if(rate2=="d"){
      im<-(num*m)/365
      ik<-(1+im)^(k/m)-1
      rate<-ik/(1+ik)
    } else if(rate2=="j"){
      im<-(num*m)/365
      ik<-(1+num)^(k/m)-1
      rate<-(ik*365)/k
    }else if(rate2=="f"){
      im<-(num*m)/365
      ik<-(1+im)^(k/m)-1
      dk<-ik/(1+ik)
      rate<-(dk*365)/k
    } else{
      stop("Check rate2")
    }
  }else if(rate1=="f"){
    if(rate2=="i"){
      dm<-(num*m)/365
      dk<-1-(1-dm)^(k/m)
      rate<-dk/(1-dk)
    } else if(rate2=="d"){
      dm<-(num*m)/365
      rate<-1-(1-dm)^(k/m)
    } else if(rate2=="j"){
      dm<-(num*m)/365
      dk<-1-(1-dm)^(k/m)
      ik<-dk/(1-dk)
      rate<-(ik*365)/k
    }else if(rate2=="f"){
      dm<-(num*m)/365
      dk<-1-(1-dm)^(k/m)
      rate<-(dk*365)/k
    } else{
      stop("Check rate2")
    }
  }else{
    stop("Check rate1")
  }
  } else if(type=="frac"){
    if(rate1=="i"){
      if(rate2=="i"){
        rate<-((1+num)^(m/k))-1
      } else if(rate2=="d"){
        ik<-((1+num)^(m/k))-1
        rate<-ik/(1+ik)
      } else if(rate2=="j"){
        ik<-((1+num)^(m/k))-1
        rate<-ik*k
      }else if(rate2=="f"){
        ik<-((1+num)^(m/k))-1
        dk<-ik/(1+ik)
        rate<-dk*k
      } else{
        stop("Check rate2")
      }
    }else if(rate1=="d"){
      if(rate2=="i"){
        dk<-1-(1-num)^(m/k)
        rate<-dk/(1-dk)
      } else if(rate2=="d"){
        rate<-1-(1-num)^(m/k)
      } else if(rate2=="j"){
        dk<-1-(1-num)^(m/k)
        ik<-dk/(1-dk)
        rate<-ik*k
      }else if(rate2=="f"){
        dk<-1-(1-num)^(m/k)
        rate<-dk*k
      } else{
        stop("Check rate2")
      }
    }else if(rate1=="j"){
      if(rate2=="i"){
        im<-num/m
        rate<-(1+im)^(m/k)-1
      } else if(rate2=="d"){
        im<-num/m
        ik<-(1+im)^(m/k)-1
        rate<-ik/(1+ik)
      } else if(rate2=="j"){
        im<-num/m
        ik<-(1+num)^(m/k)-1
        rate<-ik*k
      }else if(rate2=="f"){
        im<-num/m
        ik<-(1+im)^(m/k)-1
        dk<-ik/(1+ik)
        rate<-dk*k
      } else{
        stop("Check rate2")
      }
    }else if(rate1=="f"){
      if(rate2=="i"){
        dm<-num/m
        dk<-1-(1-dm)^(m/k)
        rate<-dk/(1-dk)
      } else if(rate2=="d"){
        dm<-num/m
        rate<-1-(1-dm)^(m/k)
      } else if(rate2=="j"){
        dm<-num/m
        dk<-1-(1-dm)^(m/k)
        ik<-dk/(1-dk)
        rate<-ik*k
      }else if(rate2=="f"){
        dm<-num/m
        dk<-1-(1-dm)^(m/k)
        rate<-dk*k
      } else{
        stop("Check rate2")
      }
    }else{
      stop("Check rate1")
    }
  }else{
    stop("Check type")
  }
  return(as.numeric(rate))
}


