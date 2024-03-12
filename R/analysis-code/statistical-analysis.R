---
#### Exploratory Data Analysis: Project Part 2
#### Kevin Kosewick & Emma Hardin-Parker

# Loading Necessary Packages 
library(ggplot2) 
library(broom) 
library(here) 
library(tidymodels)
library(tidyverse)

# Writing new dataframe, sleepdata, into a separate excel file to ensure reproducibility 

### i'm gonna need your help on this part, Kevin I tried and couldn't figure out the best way to do it ####


### For now, I'm just going to use the sleepdata frame we already have in the environment

######################################
# Fitting Models and Statistical Analysis # 
######################################

############################
#### Fitting a model using Quality of Sleep as the outcome and Physical Activity Level as a predictor

lmfit_quality_activity <- lm(Quality.of.Sleep ~ Physical.Activity.Level, sleepdata)  

# Placing results from lmfit_quality_activty  into a data frame with the tidy function
lmtable_quality_activity <- broom::tidy(lmfit_quality_activity)

# Viewing the results from the first model fit 
print(lmtable_quality_activity)

# Saving the lmfit_quality_activity results table  
lmtable_quality_activity = here("results", "tables", "lmfit1table.rds")
saveRDS(lmtable_quality_activity, file = lmtable_quality_activity)


      ## include interpretation 

############################
#### Fitting a model using Quality of Sleep as the outcome and Sleep Duration as a predictor

lmfit_quality_duration <- lm(Quality.of.Sleep ~ Sleep.Duration, sleepdata)  

# Placing results from lmfit_quality_duration into a data frame with the tidy function
lmtable_quality_duration <- broom::tidy(lmfit_quality_duration)

#Viewing the results from the second model fit 
print(lmtable_quality_duration)

# Saving the lmfit_quality_duration results table  
lmtable_quality_duration = here("results", "tables", "lmfit2table.rds")
saveRDS(lmtable_quality_duration, file = lmtable_quality_duration)

      ## include interpretation 

############################
#### Fitting a model using Quality of Sleep as the outcome and Sleep Duration and Occupation as predictors

lmfit_quality_duration_occupation <- lm(Quality.of.Sleep ~ Sleep.Duration + Occupation, sleepdata)  

# Placing results from lmfit_quality_duration into a data frame with the tidy function
lmtable_quality_duration_occupation <- broom::tidy(lmfit_quality_duration_occupation)

#Viewing the results from the third model fit 
print(lmtable_quality_duration_occupation)

# Saving the lmfit_quality_duration_occupation results table  
lmtable_quality_duration_occupation = here("results", "tables", "lmfit3table.rds")
saveRDS(lmtable_quality_duration_occupation, file = lmtable_quality_duration_occupation)

      ## include interpretation 

############################
#### Fitting a nonlinear model using the relationship between Quality of Sleep as the outcome and Sleep Duration and Occupation as predictors

#### I will be finishing this Wednesday morning --> plan to fit a nonlinear model on a scatterplot and then use the nls() function from the tidymodels resource


