library(readr)
library(dplyr)
library(hmeasure)
library(caret)

source("parameter-settings.R")
source("confusion-matrix-functions.R")

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
         'delayed+decaying' = "delayed + decaying", #sprintf("delayed + decaying (delay = %d, std = %d, rate = %g)", 
                              #        row_results$mu, 
                              #        row_results$sigma, 
                              #        row_results$rate),
         'long-term' = "long term")#sprintf("long term (rate = %g, delay = %d)", row_results$rate, row_results$delay))  
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

revert_to_index <- function(label, truth = TRUE) { 
  if (truth) {
    index <- switch(
      label,
      "no assocation" = 1,
      "current use" = 2     ,
      "past use (delay = 5)" = 3,
      "past use (delay = 10)" = 4,
      "withdrawal (rate = 0.5)" = 5,
      "withdrawal (rate = 1)" = 6     ,
      "delayed (delay = 2, std = 2)" = 7,
      "delayed (delay = 5, std = 2)" = 8  ,
      "decaying (rate = 0.5)" = 9          ,
      "decaying (rate = 1)" = 10             ,
      "delayed + decaying" = 11,# (delay = 10, std = 2, rate = 0.5)" = 11,
      "long term" = 12 # (rate = 0.25, delay = 50)" = 12
    ) 
  } else {
    index <- switch(
      label,
      "no assocation" = 1,
      "current use" = 2,
      "past use" = 3,
      "withdrawal" = 4,
      "delayed" = 5,
      "decaying" = 6,
      "delayed + decaying" = 7,
      "long term" = 8
    )
  }
  
  return(index)
}

# go over all settings
confusion_matrices <- lapply(1:nrow(only_sim_param), function(i) { 
  temp <- truth %>% filter(n_patients == only_sim_param$n_patients[i],
                           simulation_time == only_sim_param$simulation_time[i], 
                           min_chance_drug == only_sim_param$min_chance_drug[i], 
                           avg_duration == only_sim_param$avg_duration[i], 
                           prob_guaranteed_exposed == only_sim_param$prob_guaranteed_exposed[i],
                           min_chance == only_sim_param$min_chance[i],
                           max_chance == only_sim_param$max_chance[i])  
  
  n_replications <- nrow(temp) / 12
  
  confusion_matrix <- matrix(rep(0, 8*12), nrow = 8) 
  rownames(confusion_matrix) <- c("no assocation", "current use", "past use","withdrawal","delayed","decaying","delayed + decaying","long term")
  colnames(confusion_matrix) <- c(
    "no assocation",
    "current use"     ,
    "past use (delay = 5)",
    "past use (delay = 10)" ,
    "withdrawal (rate = 0.5)" ,
    "withdrawal (rate = 1)"     ,
    "delayed (delay = 2, std = 2)",
    "delayed (delay = 5, std = 2)"  ,
    "decaying (rate = 0.5)"          ,
    "decaying (rate = 1)"             ,
    "delayed + decaying", # (delay = 10, std = 2, rate = 0.5)",
    "long term" # (rate = 0.25, delay = 50)"
  )
  
  for (j in 1:nrow(temp)) { 
    index_truth <- revert_to_index(temp$truth_label[j], TRUE)
    print(index_truth)
    index_selected <- revert_to_index(temp$selected_label[j], FALSE)
    print(index_selected)
    confusion_matrix[index_selected, index_truth] <- confusion_matrix[index_selected, index_truth] + 1
    #confusion_matrix[index_selected, index_truth] <- confusion_matrix[index_truth, index_selected]
  }
  confusion_matrix / n_replications * 100
})

plot_confusion_matrix(r[[1]], title = "hello")

plots <- lapply(1:nrow(only_sim_param), function(i) { 
  n_patients <- only_sim_param$n_patients[i]
  simulation_time <- only_sim_param$simulation_time[i] 
  min_chance_drug <- only_sim_param$min_chance_drug[i] 
  avg_duration <- only_sim_param$avg_duration[i] 
  prob_guaranteed_exposed <- only_sim_param$prob_guaranteed_exposed[i]
  min_chance <- only_sim_param$min_chance[i]
  max_chance <- only_sim_param$max_chance[i]
  
  if (prob_guaranteed_exposed == 1) {
    title <- sprintf("No. patients = %d (all prescribed), Min./Max. Risk = %g/%g", n_patients, 
                     min_chance, max_chance)
  } else {
    title <- sprintf("No. patients = %d, At least %d patients prescribed, Min./Max. Risk = %g/%g", n_patients, 
                   n_patients * prob_guaranteed_exposed, min_chance, max_chance)
  }
  
  filename <-
    sprintf(
      "figures/confusion_matrix_%d_%d_%g_%g_%g_%g_%g.pdf",
      n_patients                     ,
      simulation_time,
      min_chance_drug          ,
      avg_duration             ,
      prob_guaranteed_exposed,
      min_chance,
      max_chance
    )
  
  p <- plot_confusion_matrix(confusion_matrices[[i]], title = title)
  
  ggsave(filename, plot = p, width = 10, height = 6)
  return(p)
})

# ------------------------------------------------------------------------------
# plot perfect score confusion matrix plot
perfect <- confusion_matrices[[1]]
perfect[] <- 0

perfect[1,1] <- 1
perfect[2,2] <- 1
perfect[3,3] <- 1
perfect[3,4] <- 1
perfect[4,5] <- 1

perfect[4,6] <- 1
perfect[5,7] <- 1
perfect[5,8] <- 1
perfect[6,9] <- 1
perfect[6,10] <- 1

perfect[7,11] <- 1
perfect[8,12] <- 1

perfect <- perfect * 100 

p <- plot_confusion_matrix(perfect, title = "Confusion matrix with a perfect score")

ggsave("figures/confusion-matrix_perfect.pdf", plot = p, width = 10, height = 6)

#' 
#' 
#' groups <- group_map(r, function(group, ...)
#'   group)
#' 
#' 
#' 
#' expand.grid(
#'   n_patients,
#'   simulation_time ,
#'   min_chance_drug,
#'   avg_duration,
#'   prob_guaranteed_exposed,
#'   min_chance,
#'   max_chance
#' )
#' 
#' 
#' 
#' #' COLLECT THE RESULTS ---------------------------------------------------------
#' 
#' 
#' 
#' pars <- data$truth
#' res <- data$estimates
#' 
#' # combine into one big data frame
#' res <- do.call(rbind.data.frame, res)
#' 
#' tab <- dplyr::left_join(res, pars)#, by = "job.id")
#' 
#' results <- list(truth = pars,
#'                 estimates = reduceResultsList())
#' 
#' 
#' #' There are different
#' #'  * parameter settings for the simulation
#' #'  * different versions of the weight matrix
#' #'  * different lambda values
#' 
#' # go over each experiment and select the lambda1, lambda2 value that
#' # has either the best AIC, BIC, F1 score... whatever you want to do.
#' # Do not forget to set 'maximum'!
#' get_best_score_per_experiment <-
#'   function(data,
#'            var = c("aic", "bic", "F1"),
#'            maximum = FALSE) {
#'     if (maximum) {
#'       return(data %>% group_by(job.id, repl) %>% slice(which.max(get(var[1]))) %>% ungroup())
#'     } else {
#'       return(data %>% group_by(job.id, repl) %>% slice(which.min(get(var[1]))) %>% ungroup())
#'     }
#'   }
#' 
#' 
#' aic <- get_best_score_per_experiment(data, "aic", maximum = FALSE)
#' bic <- get_best_score_per_experiment(data, "bic", maximum = FALSE)
#' oracle <- get_best_score_per_experiment(data, "F1", maximum = TRUE)
#' 
#' aic$score <- "AIC"
#' bic$score <- "BIC"
#' oracle$score <- "Oracle"
#' 
#' best_scores <- do.call("rbind", list(aic, bic, oracle))
#' 
#' readr::write_rds(best_scores, "results/best-scores.rds", compress = "gz")