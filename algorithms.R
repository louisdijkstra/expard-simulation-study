#' Algorithms used in the study 


# Functions for determining the performance ------------------------------------


#' Turn a binary adjacency matrix is a single binary vector
#' 
#' @param adj_matrices A list of adjacency matrices
#' 
#' @return A single vector of all upper-triangular parts (due to symmetry) 
get_all_labels <- function(adj_matrices) { 
  # go over all graphs
  sapply(1:length(adj_matrices), function(i) { 
    adj_matrices[[i]][upper.tri(adj_matrices[[i]], diag = FALSE)]
  })  
}

#' Get all the adjacency matrices of \code{fit$truth}
#' 
#' @param truth The true graphs
#' 
#' @return A list with all adjacency matrices
get_adj_matrices_truth <- function(truth) { 
  m <- length(truth)  
  lapply(1:m, function(i) { 
    truth[[i]]$adj_matrix 
  })
}

#' Performance Measures
#' 
#' Determine TP, FN, FP and TN etc. for each lambda1 and lambda2 value
#' 
#' @param est_labels The estimated labels (binary) of the fitted model 
#' @param true_labels The labels (binary) of the true graphs
get_classification_scores <- function(est_labels, true_labels) { 
  
  # use the hmeasure package for getting the classification measures
  cl <- hmeasure::misclassCounts(true_labels, est_labels)
  
  # add the TP, FP, FN and TN values
  results <- cl$metrics
  results$TP <- cl$conf.matrix$pred.1[1]
  results$FP <- cl$conf.matrix$pred.1[2]
  results$FN <- cl$conf.matrix$pred.0[1]
  results$TN <- cl$conf.matrix$pred.0[2]
  results$total <- results$TP + results$FP + results$FN + results$TN
  results$Hamming <- results$FP + results$FN
  results$Hamming_scaled <- results$Hamming / (results$total) 
  results$F1 <- (2*results$TP) / (2*results$TP + results$FP + results$FN)
  results$F2 <- 5*(results$Precision * results$Recall) / (4*results$Precision + results$Recall)
  
  return(results)
}

#' Performance 
#' 
#' Determines the performance of a fitted model 
#' 
#' @param fit The fitted model
#' 
#' @return A dataframe with the performance for each lambda1 and lambda2 pair
get_performance <- function(fit) { 
  
  # get the true labels
  true_labels <- get_all_labels(get_adj_matrices_truth(fit$truth))
  
  # get the estimated labels for each tuning parameter setting and 
  # return the performance
  l <- lapply(1:fit$n_lambda_values, function(i) { 
    est_labels <- get_all_labels(fit$adj_matrices[[i]])
    get_classification_scores(est_labels, true_labels)
  })
  
  l <- do.call(rbind.data.frame, l)
  cbind(fit$results, l)
}







#' expard wrapper -----------------------------------------------------------------

expard_wrapper <- function(data, job, instance, ...) { 
  
  # problem parameters are in job$prob.pars
  #print(job$prob.pars)
  #print(job$algo.pars)
  #print(job$seed)
  #print(job)
  
  #print(instance)
  res <- expard::fit_all_models(instance)
  
  res$repl <- job$repl
  res$job.id <- job$job.id
  
  return(res)
  
  # TODO: Change with type of weight matrix
  #if (job$algo.pars$type_weight_matrix == "grid") { 
  lambda1 = 1:2
  lambda2 = 1:2
  #}
  
  # Apply the CVN algorithm 
  fit <- CVN::CVN(instance$data, W, lambda1 = lambda1, 
                  lambda2 = lambda2, eps = 10e-4, maxiter = 1000, verbose = TRUE)
  
  # add the truth 
  fit$truth <- instance$truth
  
  # save results 
  filename <- paste("results/", paste(c("cvnsim", job$prob.pars, job$algo.pars, job$seed, job$repl), collapse = '_'), sep = '')
  
  # store the fit and the truth in a file 
  readr::write_rds(fit, filename, compress = "gz")
  
  performance <- get_performance(fit)
  performance$repl <- job$repl
  performance$job.id <- job$job.id
  return(performance)
}

