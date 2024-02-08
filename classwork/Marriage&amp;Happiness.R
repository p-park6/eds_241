# Generating distribution of happiness before being married
N = 500
HapBefore = rnorm(N,0,1)

# Generating married individuals depending (or not) on happiness
# 0 <= a <= 1; if a = 0 then being married does not depend on happiness
a = 1; c = 0.4
ProbMar = a*pnorm(HapBefore) + (1-a)*c
IndMar = (runif(N) <= ProbMar)

# Generating happiness after being married – no effect if d = 0
d = 0.5 # treatment effect
HapAfter = HapBefore + IndMar * d + + rnorm(N,0,0.1)

#Regressing Happiness on Marriage status 
lmHapMar = lm(HapAfter ~ IndMar)
summary(lmHapMar)

# Suppose we also observe happiness before getting married  
# lm2periods = lm(HapAfter ~ HapBefore + IndMar)
# summary(lm2periods)
