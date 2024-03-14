---
#### Exploratory Data Analysis: Project Part 2
#### Kevin Kosewick & Emma Hardin-Parker

# Loading Necessary Packages 
library(ggplot2) 
library(broom) 
library(here) 
library(tidymodels)
library(tidyverse)

######################################
# Fitting Models and Statistical Analysis # 
######################################

############################
#### Fitting a model using Quality of Sleep as the outcome and Physical Activity Level as a predictor

#Load the data
sleepdata<- readRDS(here("data","processed-data","sleepdataprocessed.RDS"))

lmfit_quality_activity <- lm(Quality.of.Sleep ~ Physical.Activity.Level, sleepdata)  

# Placing results from lmfit_quality_activty  into a data frame with the tidy function
lmtable_quality_activity <- broom::tidy(lmfit_quality_activity)

# Viewing the results from the first model fit 
print(lmtable_quality_activity)

# Saving the lmfit_quality_activity results table  
lmtable_quality_activity = here("results", "tables", "lmfit1table.rds")
saveRDS(lmtable_quality_activity, file = lmtable_quality_activity)


      ## The intercept value indicates that Quality of Sleep will be 6.66 if physical activity is at 0. This has a relatively low standard error and high significance.
## The coefficient for our variable indicates that as Physical activity level increases by one unit, Quality of Sleep will increase by 0.0109. Our t-statistic is indicated as significant by the p-value but is much lower than the intercept's.
## This means that Physical Activity Level does have a measurable impact on Quality of Sleep, but it's relatively small.

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

      ## For our sleep duration model, we can see that our intercept's negative value indicates that Quality of Sleep would be very poor if individuals got no sleep. This is rational and is supported by a strong p-value and a decent t-statistic, although the standard error is a bit high. 
## For our variable coefficient, we can see that as sleep duration increases by one unit, Quality of sleep also increases by about 1. This has a very strong p-value and t-statistic, indicating a strong relationship between this predictor and our outcome of interest.

############################
#### Fitting a model using Quality of Sleep as the outcome and Sleep Duration and Occupation as predictors

lmfit_quality_duration_occupation <- lm(Quality.of.Sleep ~ Sleep.Duration + Occupation, sleepdata)  

# Placing results from lmfit_quality_duration_occupation into a data frame with the tidy function
lmtable_quality_duration_occupation <- broom::tidy(lmfit_quality_duration_occupation)

#Viewing the results from the third model fit 
print(lmtable_quality_duration_occupation)

# Saving the lmfit_quality_duration_occupation results table  
lmtable_quality_duration_occupation = here("results", "tables", "lmfit3table.rds")
saveRDS(lmtable_quality_duration_occupation, file = lmtable_quality_duration_occupation)

      ## Since Occupation is a categorical variable, interpreting our intercept value is tricky. It's hard to get good information from it since we don't have any data from unemployed people. However, the coefficients for our predictors show a variety of interesting relationships.
## Sleep Duration shows a similar relationship to our bivariate model we ran earlier. Sales representatives have the worst sleep quality by far with a strong t-statistic and p-value. Scientists, doctors, and sales persons are all about the next worst (with strong t-stats and p-values). 
## The occupations that seem to have the best sleep are teachers, lawyers, engineers and nurses. This is somewhat surprising since these are all demanding jobs that can have decently stressful work environments. 

############################
####Fitting a model using Sleep Quality as the outcome and BMI as a predictor.

lmfit_quality_BMI <- lm(Quality.of.Sleep~ BMI.Category, sleepdata)

#Placing results from lmfit_quality_BMI into a data frame with the tidy function
lmtable_quality_BMI <- broom::tidy(lmfit_quality_BMI)

#Viewing the results from the fourth model fit 
print(lmtable_quality_BMI)

# Saving the lmfit_quality_BMI results table  
lmtable_quality_BMI = here("results", "tables", "lmfit4table.rds")
saveRDS(lmtable_quality_BMI, file = lmtable_quality_BMI)

## We can see that Obese individuals have a stronger negative relationship with Sleep Quality than Overweight individuals. The lower t-statistics and p-values are likely due to fewer observations for Obese individuals in the dataset. However, it is all still statistically significant and has reasonable standard errors. 

############################
####Fitting a model using Sleep Quality as the outcome and Blood Pressure category as a predictor.

lmfit_quality_BP <- lm(Quality.of.Sleep~ cat_bp, sleepdata)

#Placing results from lmfit_quality_BP into a data frame with the tidy function
lmtable_quality_BP <- broom::tidy(lmfit_quality_BP)

#Viewing the results from the fifth model fit 
print(lmtable_quality_BP)

# Saving the lmfit_quality_BP results table  
lmtable_quality_BP = here("results", "tables", "lmfit5table.rds")
saveRDS(lmtable_quality_BP, file = lmtable_quality_BP)

## We have some interesting results for this one. Our values aren't statistically significant according to p-values, but we can see that Stage 1 Hypertension seems to have the biggest negative impact on Quality of Sleep. This is surprising since we would expect more severe hypertension to result in worse sleep, but according to our model, Stage 2 hypertension has less impact than Stage 2. 
## Normal blood pressure seems to have a small positive impact on sleep quality according to our model. All of these predictors seem to have a relatively small impact as they impact the sleep quality scale by less than one whole unit. Given our p-values and low t-statistics, however, this model should be taken with a grain of salt.

############################
####Fitting a model using Sleep Quality as the outcome and Stress Level as a predictor.

lmfit_quality_stress <- lm(Quality.of.Sleep~ Stress.Level, sleepdata)

#Placing results from lmfit_quality_stress into a data frame with the tidy function
lmtable_quality_stress <- broom::tidy(lmfit_quality_stress)

#Viewing the results from the sixth model fit 
print(lmtable_quality_stress)

# Saving the lmfit_quality_stress results table  
lmtable_quality_stress = here("results", "tables", "lmfit6table.rds")
saveRDS(lmtable_quality_stress, file = lmtable_quality_stress)

## According to our model, Stress has a strongly defined relationship with sleep quality. We can see that a value of 0 on the stress score leads to a 10 unit increase in quality of sleep. 
## The Stress Level coefficient indicates that as stress increases by one unit, sleep quality decreases by about 0.6 of a unit. The t-statistics and p-values for the values in this model are extremely strong, indicating a well-defined relationship between stress and sleep quality.

############################
####Fitting a model using Sleep Quality as the outcome and Age and Gender as predictors.

lmfit_quality_gender_age <- lm(Quality.of.Sleep~ Gender + Age, sleepdata)

#Placing results from lmfit_quality_gender_age into a data frame with the tidy function
lmtable_quality_gender_age <- broom::tidy(lmfit_quality_gender_age)

#Viewing the results from the seventh model fit 
print(lmtable_quality_gender_age)

# Saving the lmfit_quality_gender_age results table  
lmtable_quality_gender_age = here("results", "tables", "lmfit7table.rds")
saveRDS(lmtable_quality_gender_age, file = lmtable_quality_gender_age)

## According to our table, Males seem to have lower sleep quality than females by about a score of .0336. However, the t-statistic is very low and the p-value is very poor. Gender doesn't seem to be a good predictor of sleep quality.
## When age is factored with gender, we can see that quality of sleep seems to increase slightly along with age. The t-statistic and p-value are both strong for this predictor; it may be worth doing a simple bivariate analysis of age.

############################
####Fitting a model using Sleep Quality as the outcome and Age as a predictor.

lmfit_quality_age <- lm(Quality.of.Sleep~ Age, sleepdata)

#Placing results from lmfit_quality_gender_age into a data frame with the tidy function
lmtable_quality_age <- broom::tidy(lmfit_quality_age)

#Viewing the results from the seventh model fit 
print(lmtable_quality_age)

# Saving the lmfit_quality_gender_age results table  
lmtable_quality_age = here("results", "tables", "lmfit8table.rds")
saveRDS(lmtable_quality_age, file = lmtable_quality_age)

## Our model shows a strong relationship between age and quality of sleep. As Age increases, sleep quality seems to improve. Each year increase in age is predicted to have a 0.065 increase in sleep quality score.
## The intercept here is also interesting as an age of "0" is predicted to have a sleep score of 4.57, which is relatively poor. The t-statistics and p-values for these are both strong.

############################
#### Fitting a nonlinear model using the relationship between Quality of Sleep as the outcome and Sleep Duration and Occupation as predictors

#### I will be finishing this Wednesday morning --> plan to fit a nonlinear model on a scatterplot and then use the nls() function from the tidymodels resource


