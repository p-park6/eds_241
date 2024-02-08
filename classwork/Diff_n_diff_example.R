############ PREAMBLE ##############
rm(list = ls()) # clean environment

library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)
library(ggplot2)

# SET WORKING DIRECTORY

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Save your script in the same location as the data file

did <- read_csv("did_example.csv") # Read in file

############ Difference and Difference Example ##############

## Key Variable Definitions: 
# post_treat= dummy variable (=1 if observation is post treatment, 0 if not)
# treated= dummy variable (=1 if observation is treated, 0 if not treated)
# time_x_treated= dummy interaction variable (= post_treat*treated)

# Run the regression with group means
model1 <- lm(employment ~ treated + post_treat + time_x_treated, 
            data = did)
# Get summary of the model
summary(model1)

#NOTE that this regression produces the same result as above:
model2 <- lm(employment ~ treated + post_treat + treated*post_treat, 
            data = did)

## Note that we find that: 
# Intercept 23.33 is the average employment at fast food restaurants in NJ and PA prior to the new law.
# delta = mean change in employment at ff restaurants in both NJ and PA between pre treatment time and post treatment time, remember that we are measuring over time)
# alpha = mean change in employment at ff restaurants in NJ (the state treated w/policy change) between pretreatment time and post treatment time)
# b3 = This is our coefficient of interest. The effect of the updated minimum wage law on employment. 
# Increasing the minimum wage in NJ caused an increase in employment in ff restaurants, on average, increase= 2.75 employees per ff restaurant in NJ.

######## Visualize pre and post treatment values #####

### Bar plot:
did_bar <- did %>%
  mutate(group = case_when(
    treated == 0 & post_treat == 0 ~ "PA Before",
    treated == 1 & post_treat == 0 ~ "NJ Before",
    treated == 0 & post_treat == 1 ~ "PA Post Law",
    treated == 1 & post_treat == 1 ~ "NJ Post Law"
  ))

# Plot pre and post employment
ggplot(did_bar, aes(x = group, y = employment, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Employment in NJ and PA Before and After NJ Minimum Wage Increase",
       x = "Group",
       y = "Employment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Scatterplot to visualize parallel trends
did_scatter <- did %>%
  mutate(time_period = case_when(
    post_treat == 0 ~ "Before",
    post_treat == 1 ~ "Post Law"),
    state = case_when(
      treated == 0 ~ "PA",
      treated == 1 ~ "NJ")
  ) %>%
  mutate(time_period = factor(time_period, levels = c("Before", "Post Law")),
         state = factor(state, levels = c("PA", "NJ")))

# Adding simulated NJ without treatment data points
did_scatter <- did_scatter %>%
  add_row(obs = 5, employment = 20.4, constant = 1, post_treat = 0, treated = 1, time_x_treated = 0, time_period = "Before", state = "NJ simulated") %>% # Same as our initial NJ pretreatment 
  add_row(obs = 6, employment = 18.3, constant = 1, post_treat = 1, treated = 1, time_x_treated = 0, time_period = "Post Law", state = "NJ simulated") # Simple subtraction to 'simulate parallel trends

# Plot with actual and simulated trends
ggplot(did_scatter, aes(x = time_period, y = employment, color = state, group = state)) +
  geom_point(size = 3) +
  geom_line(aes(linetype = state), size = 1) +
  geom_line(data = subset(did_scatter, state == "NJ simulated"), aes(x = time_period, y = employment, group = state), color = "green", size = 1, linetype = "dashed") +
  labs(title = "Employment in NJ and PA Before and After NJ Minimum Wage Increase\n Including NJ Parallel Trends Extrapolation",
       x = "Time Period",
       y = "Employment") +
  scale_color_manual(values = c("PA" = "blue", "NJ" = "red", "NJ simulated" = "green")) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())








