#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(tidyverse)
library(caret) 
set.seed(22) #for reproducibility and to ensure the analysis gets the same numbers each time

#Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>% #using the haven package, we need read_sav to read in an SPPS file
  mutate_all(~ifelse(.==0, NA, .)) %>% #using this to convert 0 to missing values
  filter(!is.na(MOSTHRS)) %>% #using filter to remove anyone who has a missing value
  rename("work hours" = MOSTHRS) %>% #using rename to change the title from "MOSTHRS" to "work hours" per the instructions
  select(-HRS1, -HRS2) %>% #using select with the minus symbol because it tells R to take out the variables with HRS1 and HRS2 since we now have MOSTHRS and will be working with that information
  select(-where(~mean(is.na(.))>.75)) %>% #based on suggestions from chatgpt, this select with the minus symbol allows us to keep all variables with less than 75% missingness
  mutate_all(as.numeric) #it is important to change all variables to numeric for the analyses we will run later because initally they were read in as 'unknown' which would lead to problems with future work such as our visualization or analysis
  
#Visualization
ggplot(gss_tbl, aes(x = `work hours`)) + #based on the desire to look at the univarfiate distrivution, i used a histogram and put 'work hours' in back ticks because there is a space between the two words and without it the plot would not run
  geom_histogram(binwidth = 5, fill = "pink") + #decided to make it pink
  labs(title = "Distribution of Work Hours", x = "Work Hours", y = "Frequency") #added label for clairty

#Analysis 

###Randomly order data
rows <- sample(nrow(gss_tbl)) #followed based on datacamp suggestions
shuffled_gsstbl <-gss_tbl[rows, ] #followed based on datacamp suggestions

###Determine row to split on: split
split_gss <- round(nrow(shuffled_gsstbl)* 0.75) #followed based on datacamp suggestions

###Create train
train_gss <- shuffled_gsstbl[1:split_gss, ] #followed based on datacamp suggestions

###Create test
test_gss <- shuffled_gsstbl[(split_gss + 1):nrow(shuffled_gsstbl), ] #followed based on datacamp suggestions
cv_10_folds <- createFolds(train_gss$`work hours`, 10) #followed based on datacamp suggestions
myControl <- trainControl( #followed based on datacamp suggestions
  method = "cv",
  indexOut = cv_10_folds,
  number = 10,
  verboseIter = TRUE
)

##OLS Regression
ols_model <- train( 
  `work hours` ~ ., #running work hours on all other variables
  data = train_gss, #we want to use the train_gss for shuffled data based on the split
  method = "lm", #using lm because it is the OLS model
  metric = "Rsquared", #used based on asking for the publication section
  preProcess = "medianImpute", #used based on the assignment instructions
  na.action = na.pass, #used so the model will run
  trControl = myControl #used based on datacamp suggestions
  )

##Elastic Net
en_model <- train(`work hours` ~ ., #running work hours on all other variables
  train_gss, #we want to use the train_gss for shuffled data based on the split
  tuneGrid = expand.grid( #we need to tunegrid, however, i dont have an actual reason for why i used those numbers
    alpha = 1, lambda = .1),
  method = "glmnet", #used 'glmnet' based on datacamp for the elastic net model
  preProcess = "medianImpute",#used based on the assignment instructions
  na.action = na.pass, #used so the model will run
  trControl = myControl) #used based on datacamp suggestions

##Random Forest
tuneGrid <- data.frame( #we need to tunegrid, however, i dont have an actual reason for why i used those numbers
  .mtry = 520, #couldn't get it to run in the model so created the tuneGrid outside of it first
  .splitrule = "variance",
  .min.node.size = 5
)

rf_model <- train(
  `work hours` ~ .,#running work hours on all other variables
  train_gss, #we want to use the train_gss for shuffled data based on the split
  tuneGrid = tuneGrid,
  method = "ranger", #used based on datacamp for the random forest model
  preProcess = "medianImpute", #used based on the assignment instructions
  na.action = na.pass, #used so the model will run
  trControl = myControl) #used based on datacamp suggestions

##eXtreme Boosting
eb_model <- train(
  `work hours` ~ .,#running work hours on all other variables
  train_gss, #we want to use the train_gss for shuffled data based on the split
  method = "xgbLinear", #i couldn't find this in datacamp but chatgpt gave two options so i used this one for the eXtreme Boosting Gradient model
  tuneGrid = expand.grid(nrounds = 50, alpha = 1, lambda = .1, eta = 0.1), #we need to tunegrid, however, i dont have an actual reason for why i used those numbers
  preProcess = "medianImpute", #used based on the assignment instructions
  na.action = na.pass, #used so the model will run
  trControl = myControl) #used based on datacamp suggestions

#Publication
ols_predict <- predict(ols_model, test_gss, na.action = na.pass) #we use the predict() function to make predictions from the new data and we predict on the test set that we did not use to train the model
en_predict <- predict(en_model, test_gss, na.action = na.pass) #we use the predict() function to make predictions from the new data and we predict on the test set that we did not use to train the model
rf_predict <- predict(rf_model, test_gss, na.action = na.pass) #we use the predict() function to make predictions from the new data and we predict on the test set that we did not use to train the model
eb_predict <- predict(eb_model, test_gss, na.action = na.pass) #we use the predict() function to make predictions from the new data and we predict on the test set that we did not use to train the model

ho_ols <- cor(ols_predict, test_gss$`work hours`)^2 #we use this test to calculate the r-squared values for each model
ho_en <- cor(en_predict, test_gss$`work hours`)^2 #we use this test to calculate the r-squared values for each model
ho_rf <- cor(rf_predict, test_gss$`work hours`)^2 #we use this test to calculate the r-squared values for each model
ho_eb <- cor(eb_predict, test_gss$`work hours`)^2 #we use this test to calculate the r-squared values for each model

table1_tbl <-
  tibble(algo = c("OLS regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
         cv_rsq = c(sub("^0", "",formatC(ols_model$results$Rsquared[1], format = 'f', digits = 2)), #using the R2 from the ols model while including the sub function to take out the leading zeros and only have the output to the hundreths place
                    sub("^0", "",formatC(en_model$results$Rsquared[1], format = 'f', digits = 2)), #using the R2 from the ols model while including the sub function to take out the leading zeros and only have the output to the hundreths place
                    sub("^0", "",formatC(rf_model$results$Rsquared[1], format = 'f', digits = 2)), #using the R2 from the ols model while including the sub function to take out the leading zeros and only have the output to the hundreths place
                    sub("^0", "",formatC(eb_model$results$Rsquared[1], format = 'f', digits = 2))), #using the R2 from the ols model while including the sub function to take out the leading zeros and only have the output to the hundreths place
         ho_rsq = c(sub("^0", "",formatC(ho_ols, format = 'f', digits = 2)), #using the R2 from the ols model while including the sub function to take out the leading zeros and only have the output to the hundreths place
                    sub("^0", "",formatC(ho_en, format = 'f', digits = 2)), #using the R2 from the ols model while including the sub function to take out the leading zeros and only have the output to the hundreths place
                    sub("^0", "",formatC(ho_rf, format = 'f', digits = 2)), #using the R2 from the ols model while including the sub function to take out the leading zeros and only have the output to the hundreths place
                    sub("^0", "",formatC(ho_eb, format = 'f', digits = 2)))) #using the R2 from the ols model while including the sub function to take out the leading zeros and only have the output to the hundreths place

print(table1_tbl) #print the table to review the results of the tibble i created

#Answers
##Q1: The results changed between models as the final model 10-fold CV R2 got larger with the biggest R2 in the eXtreme Gradient Boosting. This may be because there was greater hyperparameter tuning, model complexity, and explaining the variance better.
##Q2: The results showed variation with the hold out CV having lower R2 compared to the k-hold CV. This may be because of the iterations and the conservativeness of the models (but honestly im not quite sure)
##Q3: I think that the eXtreme Gradient Boosting would be good for real life prediction problem because it demonstrated high R2 values, can handle linearity, and can be optimized well with more hyperparameter tuning. However, it require a lot of computational resources, needs greater time to run, and quite complex.