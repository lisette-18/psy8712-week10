#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(tidyverse)
library(caret)

#Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>%
  mutate_all(~ifelse(.==0, NA, .)) %>% 
  filter(!is.na(MOSTHRS)) %>%
  rename("work hours" = MOSTHRS) %>%
  select(-HRS1, -HRS2) %>%
  select(-where(~ mean(is.na(.)) > 0.75)) %>%
  mutate_all(as.numeric)
  
  

#Visualization

#Analysis 


#Publication