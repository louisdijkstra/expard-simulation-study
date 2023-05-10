library(readr)
library(dplyr)
library(hmeasure)
library(caret)


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

# add a good label for every risk model ----------------------------------------
return_true_risk_model_label <- function(row_results) { 
  switch(as.character(row_results$risk_model), 
         'no-association' = "no assocation", 
         'current-use' = "current use", 
         'past-use' = sprintf("past use (delay = %d)", row_results$past),
         'withdrawal' = sprintf("withdrawal (rate = %g)", row_results$rate),
         'delayed' = sprintf("delayed (delay = %d, std = %d)", row_results$mu, row_results$sigma),
         'decaying' = sprintf("decaying (rate = %g)", row_results$rate),
         'delayed+decaying' = sprintf("delayed + decaying (delay = %d, std = %d, rate = %g)", 
                                      row_results$mu, 
                                      row_results$sigma, 
                                      row_results$rate),
         'long-term' = sprintf("long term (rate = %g, delay = %d)", row_results$rate, row_results$delay))  
}

return_selected_risk_model_label <- function(row_results) { 
  switch(as.character(row_results$selected_model), 
         'no-association' = "no assocation", 
         'current-use' = "current use", 
         'past-use' = "past use",
         'withdrawal' = "withdrawal",
         'delayed' = "delayed",
         'decaying' = "decaying",
         'delayed+decaying' = "delayed + decaying",
         'long-term' = "long term")  
}

truth$truth_label <- sapply(1:nrow(truth), function(i) return_true_risk_model_label(truth[i,]))
truth$selected_label <- sapply(1:nrow(truth), function(i) return_selected_risk_model_label(truth[i,]))

# Go over all simulation parameter settings and determine the performance ------
# only_sim_param contains all these parameters

# go over all settings
r <- lapply(1:nrow(only_sim_param), function(i) { 
  temp <- truth %>% filter(n_patients == only_sim_param$n_patients[i],
                           simulation_time == only_sim_param$simulation_time[i], 
                           min_chance_drug == only_sim_param$min_chance_drug[i], 
                           avg_duration == only_sim_param$avg_duration[i], 
                           prob_guaranteed_exposed == only_sim_param$prob_guaranteed_exposed[i],
                           min_chance == only_sim_param$min_chance[i],
                           max_chance == only_sim_param$max_chance[i])  
  
  confusion_matrix <- matrix(rep(0, 8*12), nrow = 8) 
  rownames(confusion_matrix) <- c("no assocation", "current use", "past use","withdrawal","delayed","decaying","delayed + decaying","long term")
  colnames(confusion_matrix) <- c("")
  
  for (j in 1:nrow(temp)) { 
    confusion_matrix[temp$truth_label[j], temp$selected_label[j]] <- confusion_matrix[temp$truth_label[j], temp$selected_label[j]] + 1
    confusion_matrix[temp$selected_label[j], temp$truth_label[j]] <- confusion_matrix[temp$truth_label[j], temp$selected_label[j]]
  }
  
  #m <- matrix(rep(0, 3*4), nrow = 3)
  #rownames(m) <- letters[1:3]
  #colnames(m) <- letters[1:4]
  #m['b', 'c'] <- 1
  
  #true_model <- temp$risk_model
  #selected_model <- as.factor(temp$selected_model)
  
  # attributes(selected_model) <- attributes(true_model) 
  
  confusion_matrix 
  # get the results for detecting a signal or not
  # hmeasure::HMeasure(temp$effect, temp$signal)
})

groups <- group_map(r, function(group, ...) group)



expand.grid(
  n_patients,
  simulation_time ,  
  min_chance_drug, 
  avg_duration, 
  prob_guaranteed_exposed, 
  min_chance, 
  max_chance
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