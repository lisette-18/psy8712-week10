#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(tidyverse)
library(caret)
set.seed(22) #for reproducibility and to ensure the analysis gets the same numbers each time

#Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>%
  mutate_all(~ifelse(.==0, NA, .)) %>% 
  filter(!is.na(MOSTHRS)) %>%
  rename("work hours" = MOSTHRS) %>%
  select(-HRS1, -HRS2) %>%
  select(-where(~mean(is.na(.))>.75)) %>%
  mutate_all(as.numeric)
  
#Visualization
ggplot(gss_tbl, aes(x = `work hours`)) +
  geom_histogram(binwidth = 5, fill = "pink") +
  labs(title = "Distribution of Work Hours", x = "Work Hours", y = "Frequency")

#Analysis 

###Randomly order data
rows <- sample(nrow(gss_tbl))
shuffled_gsstbl <-gss_tbl[rows, ]

###Determine row to split on: split
split_gss <- round(nrow(shuffled_gsstbl)* 0.75)

###Create train
train_gss <- shuffled_gsstbl[1:split_gss, ]

###Create test
test_gss <- shuffled_gsstbl[(split_gss + 1):nrow(shuffled_gsstbl), ]
cv_10_folds <- createFolds(train_gss$`work hours`, 10)
myControl <- trainControl(
  method = "cv",
  indexOut = cv_10_folds,
  number = 10,
  verboseIter = TRUE
)

##OLS Regression
ols_model <- train(
  `work hours` ~ .,
  data = train_gss, 
  method = "lm",
  metric = "Rsquared",
  preProcess = "medianImpute",
  na.action = na.pass,
  trControl = myControl
  )

##Elastic Net
en_model <- train(`work hours` ~ .,
  train_gss, 
  tuneGrid = expand.grid(
    alpha = 1, lambda = .1),
  method = "glmnet",
  preProcess = "medianImpute",
  na.action = na.pass,
  trControl = myControl)

##Random Forest
tuneGrid <- data.frame(
  .mtry = 520,
  .splitrule = "variance",
  .min.node.size = 5
)

rf_model <- train(
  `work hours` ~ .,
  train_gss, 
  tuneGrid = tuneGrid,
  method = "ranger",
  preProcess = "medianImpute",
  na.action = na.pass,
  trControl = myControl)

##eXtreme Boosting
eb_model <- train(
  `work hours` ~ .,
  train_gss, 
  method = "xgbLinear",
  tuneGrid = expand.grid(nrounds = 50, alpha = 1, lambda = .1, eta = 0.1),
  preProcess = "medianImpute",
  na.action = na.pass,
  trControl = myControl)

#Publication
ols_predict <- predict(ols_model, test_gss, na.action = na.pass)
en_predict <- predict(en_model, test_gss, na.action = na.pass)
rf_predict <- predict(rf_model, test_gss, na.action = na.pass)
eb_predict <- predict(eb_model, test_gss, na.action = na.pass)

ho_ols <- cor(ols_predict, test_gss$`work hours`)^2
ho_en <- cor(en_predict, test_gss$`work hours`)^2
ho_rf <- cor(rf_predict, test_gss$`work hours`)^2
ho_eb <- cor(eb_predict, test_gss$`work hours`)^2

table1_tbl <-
  tibble(algo = c("OLS regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting"),
         cv_rsq = c(sub("^0", "",formatC(ols_model$results$Rsquared[1], format = 'f', digits = 2)), 
                    sub("^0", "",formatC(en_model$results$Rsquared[1], format = 'f', digits = 2)),
                    sub("^0", "",formatC(rf_model$results$Rsquared[1], format = 'f', digits = 2)),
                    sub("^0", "",formatC(eb_model$results$Rsquared[1], format = 'f', digits = 2))),
         ho_rsq = c(sub("^0", "",formatC(ho_ols, format = 'f', digits = 2)), 
                    sub("^0", "",formatC(ho_en, format = 'f', digits = 2)),
                    sub("^0", "",formatC(ho_rf, format = 'f', digits = 2)),
                    sub("^0", "",formatC(ho_eb, format = 'f', digits = 2))))

print(table1_tbl)

#Answers
##Q1: The results changed between models as the final model 10-fold CV R2 got larger with the biggest R2 in the eXtreme Gradient Boosting. This may be because there was greater hyperparameter tuning, model complexity, and explaining the variance better.
##Q2: The results showed variation with the hold out CV having lower R2 compared to the k-hold CV. This may be because of the iterations and the conservativeness of the models (but honestly im not quite sure)
##Q3: I think that the eXtreme Gradient Boosting would be good for real life prediction problem because it demonstrated high R2 values, can handle linearity, and can be optimized well with more hyperparameter tuning. However, it require a lot of computational resources, needs greater time to run, and quite complex.