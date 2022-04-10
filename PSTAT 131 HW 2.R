library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(ggthemes)
library(ISLR)
data("pstat_131_hw_2_dataset")

#QUESTION 1
pstat_131_hw_2_dataset %>%
  mutate(age) = (pstat_131_hw_2_dataset$rings + 1.5) 

pstat_131_hw_2_dataset$rings

