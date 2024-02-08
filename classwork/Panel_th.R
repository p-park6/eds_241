library(stargazer)
library(estimatr)
library(ggplot2)
library(plm)
library(lmtest)


# SET WORKING DIRECTORY
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Save your script in the same location as the data file

# IMPORT CSV DATA and CREATE SUBSET for 1988 and 1982
SWTF <- read_csv("winter_2024/eds_241/classwork/data/Panel_SW_TrafficFatality.csv")
SWTF88 <- subset(SWTF, year == "1988")
SWTF82 <- subset(SWTF, year == "1982")


# SUMMARY STATISTICS of the variables
stargazer(SWTF, type="text", digits=2)


# BASIC CROSS-SECTIONAL OLS REGRESSION FOR YEAR 1988
m88 <- lm(formula = frate ~ beertax, data=SWTF88)
# this extracts the standard errors for the estimate and does a significance test
se_m88 <- starprep(m88, stat = c("std.error"), se_type = "HC2") 
stargazer(m88, se = se_m88, type="text")


# SCATTERPLOT with REGRESSION LINE
ggplot(data=SWTF88, aes(x=beertax, y=frate)) +
  geom_smooth(method=lm) + theme_bw() +
  geom_text(aes(label=state), size = 3) 


# FIRST DIFFERENCE MODEL FOR 1988 AND 1982
# Note: this is not the typical First Difference Estimator (FD). Even though
# estimating a difference model between 88 and 82 gets rid of the fixed effect
# and thereby of the omitted variable bias associated with it, not using all 
# data is throwing away information

dfrate <- SWTF88$frate - SWTF82$frate
dbtax <- SWTF88$beertax - SWTF82$beertax
fd8882 <- lm(formula = dfrate ~ dbtax)

se_fd8882 <- starprep(fd8882, stat = c("std.error"), se_type = "HC2") 
stargazer(fd8882, se = se_fd8882, type="text")

# CONVENTIONAL FD ESTIMATÍON
# The better and conventional FD estimator is one where we use all years in the
# data for which we can subtract the values from the previous year. So if our
# original dataset has years 82 to 88, then our first differenced dataset has one
# observation less going from dfrate83_82 to dfrate88_87. We can do all relevant
# Panel model estimation with the package PLM

# ESTIMATE THE FD REGRESSION with plm

fd <- plm(frate ~ beertax,  model = "fd", data = SWTF)

# Calculate standard errors (note slightly different procedure with plm package)
se_fd <- coeftest(fd, vcov = vcovHC(fd, type = "HC2", method="white1"))[, "Std. Error"]
# Reformat standard errors for stargazer()
se_fd <- list(se_fd)
# Output results with stargazer
stargazer(fd, keep=c("beertax"), se = se_fd, type="text")

View(SWTF)
# ESTIMATE THE FD REGRESSION manually
FD_df <- SWTF %>%
  group_by(state) %>% 
  mutate(diffrate = frate-lag(frate),
         difbtax = beertax-lag(beertax))
fdman <- lm(formula = diffrate ~ difbtax, data = FD_df)
summary(fdman)
#1 dollar increase in beer tax in a state, increases fatality rate 0.0135

# Calculate standard errors
se_fdman <- coeftest(fdman, vcov = vcovHC(fdman, type = "HC2", method="white1"))[, "Std. Error"]
# Reformat standard errors for stargazer()
se_fdman <- list(se_fdman)
# Output results with stargazer
stargazer(fdman, keep=c("beertax"), se = se_fdman, type="text")

# ESTIMATE THE BASIC 'WITHIN' FIXED EFFECTS REGRESSION
# NOTE "plm" ONLY PRODUCES CLUSTER-ROBUST STANDARD ERRORS

within1 <- plm(frate ~ beertax, index = c("state", "year"), model = "within", effect = "twoways", data = SWTF)

# Calculate standard errors (note slightly different procedure with plm package)
se_within1 <- coeftest(within1, vcov = vcovHC(within1, type = "HC2", method="white1"))[, "Std. Error"]
# Reformat standard errors for stargazer()
se_within1 <- list(se_within1)
# Output results with stargazer
stargazer(within1, keep=c("beertax"), se = se_within1, type="text")



# LEAST SQUARES DUMMY VARIABLES REGRESSION
# COMPARE HETEROSKEDASTICITY AND CLUSTER-ROBUST STANDARD ERRORS
lsdv1 <- lm(formula = frate ~ beertax + as.factor(state) + as.factor(year), data=SWTF)
lsdv2 <- lm(formula = frate ~ beertax + as.factor(state) + as.factor(year), data=SWTF)

se_lsdv_Hrobust <- starprep(lsdv1, stat = c("std.error"), se_type = "HC2", alpha = 0.05) 
se_lsdv_Crobust <- starprep(lsdv2, stat = c("std.error"), se_type = "stata", clusters=SWTF$state, alpha = 0.05) 
se_list <- list(se_lsdv_Hrobust[[1]], se_lsdv_Crobust[[1]])

stargazer(lsdv1, lsdv2, keep=c("beertax"), se = se_list, type="text")

