## ----include=FALSE------------------------------------------------------------
library(DetLifeInsurance)

## -----------------------------------------------------------------------------
Px1 <- Av.(x=25, h=5, n=10,r= 1,i = 0.035, data=CSO80MANB, cap = 150000) #Actuarial Present Value of the increasing life insurance.
Net_Prem <- PremiumFrac(Px1,x=25,m=5, k=1, i=0.035, data=CSO80MANB) #Net premium to be paid at the begining of each year of the defferal term.

## -----------------------------------------------------------------------------
Prem<- a(x=45,h=0, n=20,k=2,i=0.06, data=CSO80FALB, cap=12000, assumption = "constant") #Actuarial Present Value of the life annuity, paid twice a year.
Annualized_Prem<- PremiumFrac(Prem,x=45,m=10,k=4,data=CSO80FALB, i=0.06, assumption = "constant") #Annualized  value of the premium to be paid four times a year for 10 years.
PremQuart<-Annualized_Prem/4


## -----------------------------------------------------------------------------
Prem<-av(x=45,h=7,n=13,r=0.05, i=0.045, data = CSO80FALB, cap=12000) #Actuarial present value of a varying life annuity that follows an arithmetic progression
AnnualPrem<-PremiumFrac(px1 = Prem, x=45, m=7, k=1, i=0.045,data= CSO80FALB) #Annual premium to be paid at the beginning of each year of the deferral term. 
V_av(px= AnnualPrem, x=45, h=7, n=13, r=0.05, cantprem = 7, premperyear = 1, i=0.045, data= CSO80FALB, cap=12000, t=20) #Reserve of the coverage up to the year 20

## -----------------------------------------------------------------------------
Prem<- Avg.(x=24, h=10,n=10, k=4, r=0.06, data= CSO80MALB, i=0.03, cap= 100000, assumption = "UDD", variation = "inter") #Actuarial present value of a varying life insurance according to a geometric progression
AnnualPrem<- PremiumFrac(px1 = Prem, x=24, m=10, k=1, data= CSO80MALB, i=0.03) #Annual premium to be paid at the beginning of each year of the deferral term 
V_Avg.(px =AnnualPrem, x=24, h=10, n=10, k=4, r=0.06, i=0.03, data= CSO80MALB, cantprem = 10, premperyear = 1, assumption = "UDD", variation = "inter", cap=100000, t=240) #Reserve of the coverage up to the month 240


## -----------------------------------------------------------------------------
Prem<- A.(x=40, h=0, n=15, k=1, i=0.06, data= CSO80FANB, cap=105000) #Actuarial present value of the life insurance
AnnualizedPrem<-PremiumFrac(px1 = Prem, x=40, m=15, k=4, data= CSO80FANB, i=0.06, assumption = "UDD") #Annualized value of the premium to be paid four time a year. 
V_A.(px = AnnualizedPrem/4, x=40, h=0, n=15, cantprem = 60, premperyear = 4, data=CSO80FANB, i=0.06, assumption = "UDD", t= 180, cap=105000)  #Reserve of the coverage up to the month 180

## -----------------------------------------------------------------------------
Prem<- aD(x=20, h=5, n=5, k=2, i=0.055, data= CSO80MANB, assumption = "constant", variation = "intra", cap=12000) #Actuarial present value of a decreasing life annuity
Annualized_Prem<- PremiumFrac(px1 = Prem, x=20, m=5, k=12, assumption = "constant", data=CSO80MANB, i=0.055) #Annualized value of the premium to be paid monthly
V_aD(px= Annualized_Prem/12 ,x=20,h=5,n=5, k=2, premperyear = 12, cantprem = 60, i=0.055, data= CSO80MANB, assumption = "constant", variation = "intra", cap=12000, t=120) #Reserve up to the month 120

## -----------------------------------------------------------------------------
ages<-c(20,34,36,50)
Px<-Am.(x=ages,h=5,n=30,i=0.06,data=CSO80MANB,ndeath=2,cap=200000) #Actuarial present value of a life insurnace for a group to be paid when the second death occur.
Px/am(x=ages,h=0,n=5,i=0.06,data=CSO80MANB,type="atleast",quant=3) #Annual premium to be paid when at least 3 of the group are still alive

## -----------------------------------------------------------------------------
Table_Gompertz(x0=0, omega=100, B=0.00008, C= 1.07)


## -----------------------------------------------------------------------------
MortTable<-Table_Moivre(0,100)
Px<- A.(x=35,h=0,n=20,i=0.03,data= MortTable, cap=10000) + E(x=35,n=20, i=0.03, data=MortTable, cap=10000) #A Dotal (endowment) coverage
AnnualPrem<-PremiumFrac(px1 = Px, x=35, m=10, k=6, data= MortTable, effect = "yes", assumption = "UDD") #The annualized value of the premium
AnnualPrem/6 #The premium to be paid every two months

## -----------------------------------------------------------------------------
Fractional_table(CSO58MANB, frac=3, i= 0.047, assumption = "UDD")


## -----------------------------------------------------------------------------
Loan_amortization(V0=130000,n=15,i=0.04,method = "interest_only")


## -----------------------------------------------------------------------------
Loan_amortization(V0=123000,n=12,i=0.06,alic=0.21,method="constant_installment")

## -----------------------------------------------------------------------------
Loan_amortization(V0=300000,n=10,i=0.03,ins=0.01,method = "constant_principal")


## -----------------------------------------------------------------------------
Px1<-Payment_Protection(x=35,n=15,V0=1050000,i=0.05,ip=0.055,data=CSO58MALB,type="outstanding_debt",method="constant_principal") #Actuarial present value of the coverage
Annual_Px1 <- PremiumFrac(px1=Px1,x=35,m=5, k = 1,i=0.05,data=CSO58MALB) #Annual Premium to be paid
V_Payment_Protection(px=Annual_Px1,x=35,n=15,k=1,cantprem=5,premperyear=1,i=0.05,ip=0.055,data =CSO58MALB, type="outstanding_debt",method="constant_principal",V0=1050000,t=15) #Reserve uo to the year 15



## -----------------------------------------------------------------------------
Px1 <- Payment_Protection(x=30,n=10,k=2,V0=1000000,i=0.035,ip=0.06,data=CSO80FANB,type="payments",method="constant_instalment") #Actuarial present value of the coverage
Annual_Px1 <- PremiumFrac(px1=Px1,x=30,m=3, k = 6,i=0.035,data=CSO80FANB, assumption = "UDD") #Annualized value of the premium to be paid every two months
V_Payment_Protection(px=Annual_Px1/6,x=30,n=10,k=2,cantprem=18,premperyear=6,i=0.035,ip=0.06,data =CSO80FANB, type="payments",method="constant_instalment",V0=1000000,t=120) #Reserve up to the month 120


## -----------------------------------------------------------------------------
i<-Rate_converter(0.4,"j",60,"i",365, "days") #Convert the annual nominal rate of 2 periods to a effective annual interest rate
af(0,20,i)*1200 # Calculate the present value of an annuity-due of amount $1200 paid annually for 20 years at the rate of interest of i.

## -----------------------------------------------------------------------------
i<-Rate_converter(0.07,"f",4,"i",1, "frac") #Convert the annual nominal discount rate of 4 periods to a effective annual  interest rate
sf(1,10,i)*3000 # Calculate the final value of an annuity-immediate of amount $3000 paid annually for 20 years at the rate of interest of i.

