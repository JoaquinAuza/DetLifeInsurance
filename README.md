# DetLifeInsurance

DetLifeInsurance is an R package designed to provide tools for: 
- Deterministic valuation of actuarial reserves and life insurance and annuities premiums (both constant and variable amounts supported).
- Applying fractional age assumptions to life tables, and generating new ones based on mortality laws. 
- Calculation of equivalent interest-discount rates as well as future and present value of annuities.
- Calculation of loan amortization schedule.

In addition, 47 commonly used mortality tables are included as data.

## Installation

To install from GitHub, use:

```bash
#library(devtools)
devtools::install_github("JoaquinAuza/DetLifeInsurance")
```
**Note:** package ```devtools``` must be installed. 

## Example #1

In this example, we obtain the annual net premium of a life insurance coverage of $100000 for a term of 5 years, issued
to a male of age 30, using an interest rate of 3.5%.

```{r example2}
#library(DetLifeInsurance)

LI <- A.(x=30, h=0, n=5, i = 0.035, data=CSO80MANB, cap = 100000) #Actuarial PV of the LI
Prem <- a(x=30, h=0, n=5, k=12, i=0.035, data=CSO80MANB)
Net_Premium <- LI/Prem #Net premium to be paid at the begining of each year
```
## Example #2
In this example, we obtain the value of the actuarial reserve for a life insurance coverage where the insuree pays monthly premiums during the first year. UDD assumption is used. 

```{r example2}
#library(DetLifeInsurance)

LI <- A.(x=30, h=0, n=5, i = 0.035, data=CSO80MANB, cap = 100000) 
Prem <- a(x=30, h=0, n=1, k=12, i=0.035, data=CSO80MANB, assumption="UDD")
Net_Premium <- LI/Prem
Net_Premium_monthly <- Net_Premium/12

V_A.(px=Net_Premium_monthly, x=30, h=0, n=5, k = 1, cantprem = 12,
     premperyear = 12, i = 0.035, data=CSO80MANB, assumption = "UDD", 
     cap=100000, t=60)
```

## Status
Work in progress!

- [x] Basic functionality.
- [ ] Enhance documentation.
- [ ] Better error handling.
- [ ] New functions: group insurance, stochastic valuation?...


