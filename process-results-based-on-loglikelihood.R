# Exactly the same as process results, but this time we use the log-likelihood
# to select the best fitting model. These results are currently not shown in the 
# paper

library(readr)
library(dplyr)
library(hmeasure)
library(caret)
library(reshape2)
library(ggplot2)
library(latex2exp)
library(stringr)

# LOAD THE DATA ----------------------------------------------------------------

source("parameter-settings.R")
source("plot.R")
data <- readr::read_rds("results/raw-results.rds")

# get the truth and the estimated models
truth <- data$truth
est   <- data$estimates

#' Determine different performance measures. The variable 'truth' is updated 
#' with the results

# Function for selecting the model based on the highest posterior distribution
select_best_model_given_posterior <- function(est) { 
  # temp <- est %>% filter(posterior == max(est$posterior)) 
  temp <- est %>% filter(loglikelihood == min(est$loglikelihood)) 
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
truth$effect <- sapply(truth$risk_model, function(m) return(m != "no-association"))

# store the selected model based on posterior distribution 
truth$selected_model <- sapply(est, function(e) select_best_model_given_posterior(e))

# determine whether there is a signal or not based on posterior distribution
# truth$signal <- sapply(est, function(e) signal_yes_or_no(e))
truth$signal <- truth$selected_model != 'no-association'

# ADD LABELS -------------------------------------------------------------------

# add a good label for every true risk model 
return_true_risk_model_label <- function(row_results) { 
  switch(as.character(row_results$risk_model), 
         'no-association' = "no assocation", 
         'current-use' = "current use", 
         'past-use' = sprintf("past use (past = %d)", row_results$past),
         'withdrawal' = sprintf("withdrawal (rate = %g)", row_results$rate),
         'delayed' = sprintf("delayed (delay = %d, std = %d)", row_results$mu, row_results$sigma),
         'decaying' = sprintf("decaying (rate = %g)", row_results$rate),
         'delayed+decaying' = "delayed + decaying", 
         'long-term' = "long term") 
}

# add a good label for every selected risk model 
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

# Add the labels to the truth
truth$truth_label <- sapply(1:nrow(truth), function(i) return_true_risk_model_label(truth[i,]))
truth$selected_label <- sapply(1:nrow(truth), function(i) return_selected_risk_model_label(truth[i,]))

# DETERMINE THE CONFUSION MATRICES ---------------------------------------------

#' Returns an index for the confusion matrix given the label. If truth = TRUE
#' the index for the label for the true models is returned, selected models 
#' otherwise
revert_to_index <- function(label, truth = TRUE) { 
  if (truth) {
    index <- switch(
      label,
      "no assocation" = 1,
      "current use" = 2     ,
      "past use (past = 5)" = 3,
      "past use (past = 10)" = 4,
      "withdrawal (rate = 0.5)" = 5,
      "withdrawal (rate = 1)" = 6     ,
      "delayed (delay = 2, std = 2)" = 7,
      "delayed (delay = 5, std = 2)" = 8  ,
      "decaying (rate = 0.5)" = 9          ,
      "decaying (rate = 1)" = 10             ,
      "delayed + decaying" = 11,
      "long term" = 12
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
  # filter for the relevant results
  temp <- truth %>% filter(n_patients == only_sim_param$n_patients[i],
                           simulation_time == only_sim_param$simulation_time[i], 
                           prob_exposed == only_sim_param$prob_exposed[i], 
                           avg_duration == only_sim_param$avg_duration[i], 
                           min_chance == only_sim_param$min_chance[i],
                           max_chance == only_sim_param$max_chance[i])  
  
  temp <- temp[1:240, ]
  
  n_replications <- nrow(temp) / 12
  
  # Initial the confusion matrix
  confusion_matrix <- matrix(rep(0, 8*12), nrow = 8) 
  rownames(confusion_matrix) <- c("no assocation", "current use", "past use","withdrawal","delayed","decaying","delayed + decaying","long term")
  colnames(confusion_matrix) <- c(
    "no assocation",
    "current use"     ,
    "past use (past = 5)",
    "past use (past = 10)" ,
    "withdrawal (rate = 0.5)" ,
    "withdrawal (rate = 1)"     ,
    "delayed (delay = 2, std = 2)",
    "delayed (delay = 5, std = 2)"  ,
    "decaying (rate = 0.5)"          ,
    "decaying (rate = 1)"             ,
    "delayed + decaying", 
    "long term" 
  )
  
  for (j in 1:nrow(temp)) { 
    print(temp$truth_label[j])
    print(temp$selected_label[j])
    index_truth <- revert_to_index(temp$truth_label[j], TRUE)
    index_selected <- revert_to_index(temp$selected_label[j], FALSE)
    confusion_matrix[index_selected, index_truth] <- confusion_matrix[index_selected, index_truth] + 1
  }
  
  # if you want percentages return this: 
  # return(confusion_matrix / n_replications * 100)
  
  # if you want to fill in the zero values in the plots as well, return: 
  return(confusion_matrix) 
  
  confusion_matrix[confusion_matrix == 0] <- NA
})

plot_confusion_matrix(conf_matrix = confusion_matrices[[1]], title = "title")

plots <- lapply(1:nrow(only_sim_param), function(i) { 
  n_patients <- only_sim_param$n_patients[i]
  simulation_time <- only_sim_param$simulation_time[i] 
  avg_duration <- only_sim_param$avg_duration[i] 
  prob_exposed <- only_sim_param$prob_exposed[i]
  min_chance <- only_sim_param$min_chance[i]
  max_chance <- only_sim_param$max_chance[i]
  
  title <- TeX(sprintf("Probability exposed = $%g$, $\\pi_1 = %g$, $\\pi_0 = %g$", prob_exposed,
                       max_chance, min_chance))
  
  filename <-
    sprintf(
      "figures/confusion_matrix_%d_%d_%g_%g_%g_%g.pdf",
      n_patients                     ,
      simulation_time,
      prob_exposed,
      avg_duration,
      min_chance,
      max_chance
    )
  
  p <- plot_confusion_matrix(confusion_matrices[[i]], title = title, 
                             add_legend = FALSE, 
                             leave_out_zero_values =  TRUE)
  
  
  ggsave(filename, plot = p, width = 7, height = 6)
  
  # save rds
  filename <- stringr::str_replace(filename, ".pdf", ".rds")
  
  readr::write_rds(p, filename)
  
  return(p)
})

# ------------------------------------------------------------------------------
# plot perfect score confusion matrix plot
# ------------------------------------------------------------------------------
n_replications <- 20

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

perfect <- perfect * n_replications

p <- plot_confusion_matrix(perfect, title = "Confusion matrix with a perfect score")

ggsave("figures/confusion-matrix_perfect.pdf", plot = p, width = 10, height = 6)


# ------------------------------------------------------------------------------
# Plot the true risk models 
# ------------------------------------------------------------------------------
source("plot-all-true-risk-models.R")

# ------------------------------------------------------------------------------
# Get performance measures binary
# ------------------------------------------------------------------------------

performances_per_simulation_setting = lapply(1:nrow(only_sim_param), function(i) { 
  temp <- truth %>% filter(n_patients == only_sim_param$n_patients[i],
                           simulation_time == only_sim_param$simulation_time[i], 
                           prob_exposed == only_sim_param$prob_exposed[i], 
                           avg_duration == only_sim_param$avg_duration[i], 
                           min_chance == only_sim_param$min_chance[i],
                           max_chance == only_sim_param$max_chance[i])  
  
  temp <- temp[1:240, ]
  temp_results <- hmeasure::HMeasure(temp$effect, temp$signal)
  temp_results$metrics
})

performance <- bind_rows(performances_per_simulation_setting)
rownames(performance) <- NULL

# combine performance metrics with the simulation parameter settings
performance <- cbind(only_sim_param, performance)

readr::write_rds(performance, "results/overall-performance-loglikelihood.rds")
