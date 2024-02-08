# Generating distribution of happiness before being married
N = 500
Y0 = rnorm(N,0,1)

# Defining the treatment D (getting married) 
# Two versions (one has to be commented out):
## CASE 1: Getting married is a random selection
ProbDeq1 = 0.4
D = (runif(N) <= ProbDeq1) # this is the observed treatment

## CASE 2: Probability of getting married depends on happiness before getting married
#ProbDeq1 = pnorm(Y0) # creates higher probabilities to marry for happier individuals
#D = (runif(N) <= ProbDeq1) # this is the observed treatment

# Adding the treatment effect to those being married
beta1 = 0.5 # constant treatment effect = Average Treatment Effect (ATE)
Y = Y0 + D * beta1 # this is the outcome (happiness) we observe 

#Regressing Happiness on Marriage status 
lmATEestimation = lm(Y ~ D)
summary(lmATEestimation)