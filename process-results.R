library(readr)
library(dplyr)

# Function that take in the raw results stored 
data <- readr::read_rds("results/raw-results.rds")

#' There are different 
#'  * parameter settings for the simulation
#'  * different versions of the weight matrix
#'  * different lambda values

# go over each experiment and select the lambda1, lambda2 value that 
# has either the best AIC, BIC, F1 score... whatever you want to do. 
# Do not forget to set 'maximum'!
get_best_score_per_experiment <- function(data, var = c("aic", "bic", "F1"), maximum = FALSE) { 
  if (maximum) { 
    return(
      data %>% group_by(job.id, repl) %>% slice(which.max(get(var[1]))) %>% ungroup()
    )
  } else {
    return(
      data %>% group_by(job.id, repl) %>% slice(which.min(get(var[1]))) %>% ungroup()
    )
  }
}


aic <- get_best_score_per_experiment(data, "aic", maximum = FALSE)
bic <- get_best_score_per_experiment(data, "bic", maximum = FALSE)
oracle <- get_best_score_per_experiment(data, "F1", maximum = TRUE)

aic$score <- "AIC"
bic$score <- "BIC"
oracle$score <- "Oracle"

best_scores <- do.call("rbind", list(aic, bic, oracle))

readr::write_rds(best_scores, "results/best-scores.rds", compress = "gz")