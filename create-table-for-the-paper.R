# create table for the paper

library(dplyr)
library(readr)
library(xtable)

res <- readr::read_rds("results/overall-performance.rds")
colnames(res)
table <- res %>% select(prob_exposed, min_chance, max_chance, AUC, Sens, Spec, F) %>% 
  arrange(-prob_exposed, -min_chance, -max_chance)

print(xtable(table),floating=FALSE,latex.environments=NULL)
