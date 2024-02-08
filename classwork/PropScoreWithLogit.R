#########################################################################################
# This code replicates the example provided by Olmos and Govindasamy (2015) on
# propensity score matching. 
# See https://journals.sfu.ca/jmde/index.php/jmde_1/article/view/431.
# For a short description of the dataset and original sources see
# https://search.r-project.org/CRAN/refmans/designmatch/html/lalonde.html
# Created by: Thomas Heckelei
# Date: January 24, 2024
#######################################################################################

# Clear environment
rm(list = ls())
# Disable scientific notation
options(scipen=999)

# Load libraries
 # install.packages("MatchIt")
 # install.packages("RItools")
 # install.packages("Hmisc")
library("MatchIt") # For matching
library("RItools")
library("Hmisc")
library("tidyr")
library("dplyr")

# load data
data("lalonde")
N=length(lalonde$treat)
NT=sum(lalonde$treat)
NC=N-NT
View(lalonde)

# ################
# Analysis follows
# ################

# --- Computing	indices	of covariate	imbalance	before	matching
# you will see that the tests reject the balanced distribution across all 
# variables simultaneously (chi-square test) and for most variables separately
# (std.diffs test) 
xBalance(treat	~ age	+ educ + nodegree	+ re74	+ re75,	data=lalonde, # note: treat = Di in slide notation
         report=c("std.diffs","chisquare.test", "p.values"))

###############
# shows that they are not good comparisons, not BALANCED
#mean(lalonde$age)
#treat <- lalonde %>%
#  filter(treat == 1) %>%
#  summarize(., ~ mean(), n=n)
#?summarise
#mean(lalonde$age)
###############


# --- Treatment effect estimation using mean difference between treated and
# non-treated --> we get a negative income effect of training program
ATE_naive = (sum(lalonde$treat*lalonde$re78)/NT 
            - sum((1-lalonde$treat)*lalonde$re78)/NC)
ATE_naive

# You get the same if you run a simple regression on treatment without covariates
reg	<-lm(re78	~ treat,
data = lalonde)
summary(reg)

# --- Treatment effect estimation of regression coefficient of treatment 
# without matching but with controls. Now estimated treatment effect is positive 
# Why should you not trust this estimate either?
reg	<-lm(re78	~ treat	+ age	+ educ	+ nodegree	+ re74	+ re75	+ married,
        data = lalonde)
summary(reg)

# --- Estimation of propensity	scores with the glm function
# choosing family = "binomial" will pick a "logit" (or logistic) model
# see https://www.datacamp.com/tutorial/logistic-regression-R for a quick
# intro to logistic regression
ps	<- glm(treat	~ age	+ educ	+ nodegree	+ re74	+ re75,
        data	=lalonde,	family	= binomial())
summary(ps)

# --- Attach	the	predicted	propensity	score	to	the	datafile
# Note: the predictions from a logit model are probabilities
# In this case probabilities to be treated given the covariates chosen, 
# i.e., the propensity scores
lalonde$psvalue	<- predict(ps,	type	= "response")

# --- Drawing back to back histograms for propensity scores for treated and 
# non-treated before matching
histbackback(split(lalonde$psvalue,	lalonde$treat),	main= 
  "Propensity	score	before	matching",	xlab=c("control",	"treatment"))

# --- Match	using	nearest-neighbor approach, i.e. treated units are assigned the 
# non-treated unit with the closest propensity score as match 
# You will see that matches are found for all 185 treated units (guaranteed
# with the nearest-neighbor approach)
m.nn	<- matchit(treat	~ age	+ educ	+ nodegree	+ re74	+ re75,
        data	=lalonde,	method= "nearest",	ratio	= 1)
summary(m.nn)
match.data	= match.data(m.nn)

# --- Computing	indices	of covariate	imbalance	after	matching
# same command as above but using the matched data now
# what you will see is that matching by propensity scores balances
# the covariates between treated and non-treated that were used in the
# estimation of the propensity scores
xBalance(treat	~ age	+ educ + nodegree	+ re74	+ re75,	data=match.data,
         report=c("std.diffs","chisquare.test", "p.values"))

# Drawing back to back histograms for propensity scores for treated and 
# non-treated after matching
histbackback(split(match.data$psvalue,	match.data$treat),	main= "Propensity
        score	after	matching",	xlab=c("control",	"treatment"))

# --- Treatment effect estimation using average outcome difference of matched pairs
# do this with for loop but likely more elegant approaches are possible
# note that equal entries in "subclass" of match.data defines the match
# this loop sums up the differences of outcomes between matches and then 
# divides by NT to derive the mean difference as the ATE estimate

sumdiff<-0
for (i in 1:NT){
  temp <- match.data[is.element(match.data$subclass, paste0(i)),]
  sumdiff <- sumdiff + temp[1,"re78"]-temp[2,"re78"] 
}
sumdiff
ATT_m_nn = 1/NT * sumdiff
ATT_m_nn

# NOTE: This is an ATT estimate, NOT an ATE. Why? Because we picked nearest 
# neighbor matches only for the treated. So we defined a counterfactual only for the
# treated. We did not do a matching for all non-treated. Only if we had done this
# and then calculated the average outcome differences for the whole population
# between the matched pairs would we have estimated the ATE

View(match.data)

# alternative in tidyverse
sumdiff_data<-match.data%>%
  group_by(subclass)%>%
  mutate(diff=re78[treat==1]-re78[treat==0])

sumdiff<-sum(sumdiff_data$diff)/2
ATT_m_nn = 1/NT * sumdiff
ATT_m_nn


# --- estimate treatment effect with Inverse Probability Weighting estimator
# The treatment effect estimates in the following are based on the full dataset
# and differ substantially from the ATT_m_nn just calculated. This indicates the 
# strong heterogeneity between the treated and non-treated. Therefore it is 
# expected that the ATE differs from the ATT (see slides from lecture 2). 
# For the calculation of the IPW estimator see slide 23 of lecture 5

PS <- lalonde$psvalue
Y <- lalonde$re78
D <- lalonde$treat
EY1 <- D*Y/PS / sum(D/PS)
EY0 <- (1-D)*Y/(1-PS) / sum((1-D) / (1-PS))
ATE_IPW = sum(EY1) - sum(EY0)
ATE_IPW


# --- estimate treatment effect with Weighted least Squares (WLS) estimator
# Both the nearest neighbor matching estimator and the IPW estimattor do not
# easily allow to calculated standard errors. They also do not allow to take
# controls into consideration. Therefore the following weighted least squares
# estimator has advantages

# calculation of the weights - see slide 25 of lecture 5
lalonde$wgt = (D/PS + (1-D)/(1-PS))

# doing WLS without controls reproduces the IPW estimator
reg_wls	<-lm(re78	~ treat,
             data = lalonde, weights = wgt)
summary(reg_wls)

# With controls. Strongly advisable as outcomes depend on controls and including
# them allows to estimate the ATE with more precision
# --> the treatment effect has a lower standard error. Don't get confused by the
# lack of statistical significance compared to the case without controls. It is
# because the estimated treatment effect is smaller
reg_wls_c	<-lm(re78	~ treat	+ age	+ educ	+ nodegree	+ re74	+ re75	+ married,
          data = lalonde, weights = wgt)
summary(reg_wls_c)


