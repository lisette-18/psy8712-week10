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

