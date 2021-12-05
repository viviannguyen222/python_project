library(tseries)
library(zoo)
## Daily stock returns, Microsoft and IBM
p1 = get.hist.quote(instrument = "msft",start = "2010-01-01",
                    end = "2020-12-31",quote = "AdjClose")
p2 = get.hist.quote(instrument = "ibm",start = "2010-01-01",
                    end = "2020-12-31",quote = "AdjClose")
y1=coredata(diff(log(p1))) # Convert prices to returns and strings
# date information from returns
y2=coredata(diff(log(p2)))
ys1=sort(y1)
ys2=sort(y2)
value=1000
alpha=0.01
T1=length(y1)
op=ceiling(alpha*T1) # aplpha percent smallest, rounded up
## VaR of Microsoft 
VaR1=-ys1[op]*value
print(VaR1)
# Normal VaR
sigma = sd(y1) # estimate volatility
VaR2= -sigma*qnorm(alpha)*value
print(VaR2)
ES2=sigma*dnorm(qnorm(alpha))/alpha*value
print(ES2)

## Student-t VaR
library(QRM)
scy1 = (y1)*100 # scale the returns
res = fit.st(scy1)
sigma1 = unname(res$par.ests['sigma']/100) # rescale the volatility
nu = unname(res$par.ests['nu'])
## Note: We are removing the names of the fit.st output using 
VaR3 = -sigma1*qt(df=nu,p=alpha)*value
print(VaR3)

########################################
# Simulate VaR
########################################
set.seed(1)       #set seed
S = 1e7       # number of simulations
s2 = sigma^2  # daily varience
alpha = 0.01  # probability
r = 0.05      # anual riskfree rate
p = 1000      # price today
ysim = rnorm(S,r/365-0.5*s2,sqrt(s2)) # sim returns
Psim = P*exp(ysim)    # sim future prices
q = sort(Psim-P)      # simulated P/L
VaRsim = -q[alpha*S]
print(VaRsim)

