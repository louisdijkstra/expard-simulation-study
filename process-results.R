library(readr)
library(dplyr)

source("parameter-settings.R")

# Function that take in the raw results stored 
data <- readr::read_rds("results/raw-results.rds")

# get the truth and the estimated models
truth <- data$truth
est <- data$estimates

#' Determine different performance measures. The variable 'truth' is updated 
#' with the results

# Function for selecting the model based on the highest posterior distribution
select_best_model_given_posterior <- function(est) { 
  temp <- est %>% filter(posterior == max(est$posterior))  
  if (nrow(temp) == 0) { 
    return("no-association")
  }
  return(unique(temp$model))
}

# Determine whether there is a signal or not
signal_yes_or_no <- function(e) { 
  na <- e %>% filter(model == "no-association")
  prob <- na$posterior
  
  # if NA, then there is simply no signal
  if (is.na(prob)) { 
    return(FALSE)  
  }
  
  if (prob >= 0.5) { 
    return(FALSE)
  } else { 
    return(TRUE)  
  }
}

# store whether there is an effect or not (true model)
truth$effect <- sapply(truth$risk_model, function(m) return(m == "no-association"))

# store the selected model based on posterior distribution 
truth$selected_model <- sapply(est, function(e) select_best_model_given_posterior(e))

# determine whether there is a signal or not based on posterior distribution
truth$signal <- sapply(est, function(e) signal_yes_or_no(e))

r <- truth %>% group_by(n_patients, simulation_time, min_chance_drug, avg_duration, 
                   prob_guaranteed_exposed, min_chance, max_chance)

r %>% mutate()

expand.grid(
  n_patients = c(10,20),
  simulation_time = c(10),  
  min_chance_drug = c(0.05), 
  avg_duration = c(5), 
  prob_guaranteed_exposed = c(1), 
  min_chance = c(.01), 
  max_chance = c(.9), 
  risk_model = c(
    'no-association',
    'current-use',
    'past-use',
    'withdrawal',
    'delayed',
    'decaying',
    'delayed+decaying',
    'long-term'
  )
)



#' COLLECT THE RESULTS --------------------------------------------------------- 



pars <- data$truth 
res <- data$estimates

# combine into one big data frame
res <- do.call(rbind.data.frame, res)

tab <- dplyr::left_join(res, pars)#, by = "job.id")

results <- list(
  truth = pars, 
  estimates = reduceResultsList() 
)


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