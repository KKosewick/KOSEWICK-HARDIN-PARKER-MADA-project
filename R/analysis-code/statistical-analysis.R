---
#### Exploratory Data Analysis: Project Part 2
#### Kevin Kosewick & Emma Hardin-Parker

# Loading Necessary Packages 

library(broom) 
library(here) 
library(tidymodels)
library(tidyverse)
library(corrplot)
library(ranger)
library(vip)
library(ggplot2)


######################################
# Fitting Models and Statistical Analysis # 
######################################

############################
#### Fitting a model using Quality of Sleep as the outcome and Physical Activity Level as a predictor

#Load the data
sleepdata<- readRDS(here("data","processed-data","sleepdataprocessed.rds"))

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

#Placing results from lmfit_quality_age into a data frame with the tidy function
lmtable_quality_age <- broom::tidy(lmfit_quality_age)

#Viewing the results from the eighth model fit 
print(lmtable_quality_age)

# Saving the lmfit_quality_age results table  
lmtable_quality_age = here("results", "tables", "lmfit8table.rds")
saveRDS(lmtable_quality_age, file = lmtable_quality_age)

## Our model shows a strong relationship between age and quality of sleep. As Age increases, sleep quality seems to improve. Each year increase in age is predicted to have a 0.065 increase in sleep quality score.
## The intercept here is also interesting as an age of "0" is predicted to have a sleep score of 4.57, which is relatively poor. The t-statistics and p-values for these are both strong.

##############################
#### Now we'll create a new variable that groups the different occupations to make analysis more succinct.

# Create a new column
sleepdata$Occupation_Group <- as.character(sleepdata$Occupation)

# Map occupations to broader categories
sleepdata$Occupation_Group[sleepdata$Occupation %in% c('Nurse', 'Doctor')] <- 'Healthcare'
sleepdata$Occupation_Group[sleepdata$Occupation == 'Teacher'] <- 'Education'
sleepdata$Occupation_Group[sleepdata$Occupation %in% c('Software Engineer', 'Engineer')] <- 'Engineering'
sleepdata$Occupation_Group[sleepdata$Occupation %in% c('Accountant', 'Salesperson', 'Sales Representative', 'Manager')] <- 'Business/Finance'
sleepdata$Occupation_Group[sleepdata$Occupation == 'Scientist'] <- 'Science'

# Convert the new column to a factor
sleepdata$Occupation_Group <- as.factor(sleepdata$Occupation_Group)

## This should help with analysis of this variable. We'll fit this to a model now to test it.

lmfit_quality_occupation <- lm(Quality.of.Sleep~ Occupation_Group, sleepdata)

#Placing results from lmfit_quality_occupation into a data frame with the tidy function
lmtable_quality_occupation <- broom::tidy(lmfit_quality_occupation)

#Viewing the results from the occupation model fit 
print(lmtable_quality_occupation)

# Saving the lmfit_quality_gender_age results table  
lmtable_quality_occupation = here("results", "tables", "lmfit9table.rds")
saveRDS(lmtable_quality_occupation, file = lmtable_quality_occupation)

## Looking at our newly grouped occupations, we can see that our intercept has a high t-statistic and significance; this indicates that being unemployed would result in a sleep quality score of about 7, which is pretty good. 
# Teaching and healthcare professions don't seem to have a significant impact on an individual's sleep quality score. They have low t-statistics and large p-values.
# Engineers seem to have the best sleep quality score; they're predicted to have a score of about 8.3 (with strong p-values and t-statistics). Scientists seem to have the worst score; they're predicted to have 2 points lower sleep quality than our intercept. The t-statistic and p-value are both strong for this group.
# Lawyers fall in the middle, having a decent increase in sleep quality compared to the intercept and solid p-values and t-statistics to support this.

##############################
#### Now we'll remove unnecessary variables (e.g. ones we've feature engineered into more useful ones) to make our analyses easier.

sleepdatafinal <- subset(sleepdata, select = -c(Occupation, systolic, diastolic, Daily.Steps, Physical.Activity.Level, Heart.Rate, AgeGroup))

##############################
#### Creating a collinearity plot

#We'll make sure there isn't collinearity between our continuous variables before we fit more complex models. 

# Select the variables
continuous_vars <- sleepdatafinal[, c("Age", "Sleep.Duration", "Quality.of.Sleep", "Stress.Level")]

# Compute correlation matrix
correlation_matrix <- cor(continuous_vars)

# Create a pairwise correlation plot (using corrplot)
correlation <- corrplot(correlation_matrix, method = "circle")

#Save the correlation plot
correlation = here("results", "figures", "correlation.rds")
saveRDS(correlation, file = correlation)

#It seems that stress level and quality of sleep are at an absolute value of 0.9 or higher on the correlation scale. 
#Sleep Duration and stress level also seem closely tied. Stress level and quality of sleep are both subjective scales given by an individual; this is interesting to note.
#We're definitely still interested in stress as a predictor. We'll keep it in our analysis, but it seems good to note that it has a very linear relationship with sleep quality.

##############################
#### Fitting a Random Forest model and using cross validation to verify its predictive power on unseen data

#First, we'll create a random seed to aid in reproducibility.

rngseed=1234
set.seed(rngseed)

#We'll now train a Random Forest model using CV with no train/test split (as the data has less than 400 observations and many of the values are rare/unique for some columns)

# Random Forest model specification; include the mtry and min_n parameters
model_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 300) %>%
  set_engine("ranger", importance = 'impurity', seed = rngseed) %>%
  set_mode("regression")

# Specify the recipe
data_recipe <- recipe(Quality.of.Sleep ~ ., data = sleepdatafinal)

# Create Random Forest workflow
data_workflow <- workflow() %>%
  add_model(model_spec) %>%
  add_recipe(data_recipe)

# Create resamples using 5-fold cross-validation, 5 times repeated
resamples <- vfold_cv(sleepdatafinal, v = 5, repeats = 5)

# Define the grid of parameters
mtry_param <- mtry(range = c(1, 7))
min_n_param <- min_n(range = c(1, 21))
rf_grid <- grid_regular(mtry_param, min_n_param, levels = 7)

# Tune the Random Forest model
tune_results_rf <- tune_grid(
  data_workflow,
  resamples = resamples,
  grid = rf_grid
)

# Print RF results
print(tune_results_rf)
#Plot them too
autoplot(tune_results_rf)
#Save them
tune_results_rf = here("results", "figures", "tune_results_rf.rds")
saveRDS(tune_results_rf, file = tune_results_rf)

#We've successfully fit our RF model; now, we can pick the different pieces apart and see what predictors seem the most important and which model is best.

#Best RMSE model
best_params <- select_best(tune_results_rf, metric = "rmse")

#Variable importance info
# Finalize the workflow with the best parameters
final_wf <- finalize_workflow(data_workflow, best_params)

# Fit the finalized workflow to the entire data
final_fit <- fit(final_wf, data = sleepdatafinal)
# Extract the fitted model
fitted_model <- extract_fit_parsnip(final_fit)

# Create a variable importance plot
vip(fitted_model$fit)
#Save it
vip = here("results", "figures", "vip.rds")
saveRDS(vip, file = vip)

#We can see that sleep duration, stress level, and Age seem to have the most importance for our model.
#PhysicalActivityLevel doesn't seem to have much of an impact on Quality of Sleep, as it was excluded from the variable importance plot altogether. These results are pretty in line with what we discovered
# with our EDA and our simple linear models, but now we have a ranking given by this plot. 

#Now, we'll see how well this model predicts new data.
# Make predictions
predictions <- predict(final_fit, new_data = sleepdatafinal)

# Add the predictions to dataframe
sleepdatafinal$Predicted <- predictions$.pred

# Create an observed vs. predicted plot
observed_vs_predicted <-ggplot(sleepdatafinal, aes(x = Quality.of.Sleep, y = Predicted)) +
  geom_point() +
  geom_abline(color = "red") +
  labs(x = "Observed", y = "Predicted", title = "Observed vs. Predicted Plot") +
  theme_minimal()

#Save it
observed_vs_predicted = here("results", "figures", "observed_vs_predicted.rds")
saveRDS(observed_vs_predicted, file = observed_vs_predicted)
#This seems like a very solid fit; not too close to where overfitting is obvious, but not too far to where the predictions aren't useful either.

##############################
#### Creating a "null" model and comparing our RF results 

#Now, we'll make a "null" model and compare its results to that of our RF in order to make sure that the RF is useful.

# Specify the null model
null_mod <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

# Fit the null model to your data
null_fit <- fit(null_mod, Quality.of.Sleep ~ 1, data = sleepdatafinal)

# Make predictions
null_predictions <- predict(null_fit, new_data = sleepdatafinal)

# Bind the predictions with the actual values
data_with_predictions <- bind_cols(sleepdatafinal, null_predictions)

# Compute RMSE
null_rmse <- rmse(data_with_predictions, truth = Quality.of.Sleep, estimate = .pred)

# Print the RMSE
print(null_rmse)

#This RMSE is a good bit higher than the 0.2-0.32 range of our Random Forest models. This is a good sign that the RF model performs well on the data.

