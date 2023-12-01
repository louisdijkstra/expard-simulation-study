# Create the performance table for signal detection. We consider here the 
# log-likelihood to select the best model. The output is a LaTeX table

library(readr)
library(expard)
library(xtable)

res_posterior <- readr::read_rds("results/overall-performance.rds")
res_logl <- readr::read_rds("results/overall-performance-loglikelihood.rds")

colnames(res_posterior)

res_posterior <- res_posterior %>% select(
  prob_exposed, min_chance, max_chance, Precision, Recall, F
)

res_logl <- res_logl %>% select(
  Precision, Recall, F
) %>% rename(Precision_logl = Precision, Recall_logl = Recall, F_logl = F)

res <- cbind(res_posterior, res_logl) %>% arrange(prob_exposed, min_chance, max_chance)

print(xtable(res, type = "latex")) 
