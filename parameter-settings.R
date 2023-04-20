#' The Parameter Settings for the Methods and the Simulations

#' For debugging. Only a limited number of parameter settings is used, see 
#' parameter-settings.R. Only 
test_run <- TRUE  

#' Parameter settings for the algorithms ------------------
algo_param <- data.frame(
  expand.grid(
))

#' Parameter settings for the simulation study ------------

if (test_run) { # simplify the parameters for a test run - this is debugging
  
  sim_param <- dplyr::as_tibble(
    expand.grid(
      n_patients = c(10),
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
  )
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "past-use", 
      past = c(5)
    ))
  
  sim_method_all <-  sim_method
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "withdrawal", 
      rate = c(.5)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "delayed", 
      mu = c(3), 
      sigma = c(2)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "decaying", 
      rate = c(.5)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "delayed_decaying", 
      mu = c(3), 
      sigma = c(2), 
      rate = c(.5)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "long-term", 
      rate = c(.5), 
      delay = c(10)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_param <-  merge(sim_method_all, sim_param)
  
  #' provide a specific id to each parameter setting. This makes it 
  #' easier to process the results later on when there are repetitions
  sim_param$sim_param_id <- 1:nrow(sim_param)
  
} else { # Not a test run 
  
  sim_param <- dplyr::as_tibble(
    expand.grid(
      n_patients = c(100, 1000, 10000),
      simulation_time = c(20, 100),  
      min_chance_drug = c(0.05), 
      avg_duration = c(5), 
      prob_guaranteed_exposed = c(.1, .5, 1), 
      min_chance = c(.01, 0.05), 
      max_chance = c(.1, .25, .5, .9), 
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
  )
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "past-use", 
      past = c(5, 10)
    ))
  
  sim_method_all <-  sim_method
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "withdrawal", 
      rate = c(.5, 1, 2)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "delayed", 
      mu = c(3, 10, 20), 
      sigma = c(2)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "decaying", 
      rate = c(.5, 1, 2)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "delayed_decaying", 
      mu = c(3, 10, 20), 
      sigma = c(2), 
      rate = c(.5, 1, 2)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_method <- dplyr::as_tibble(
    expand.grid(
      risk_model = "long-term", 
      rate = c(.5, 1, 2), 
      delay = c(10, 20, 50)
    ))
  
  sim_method_all <-  dplyr::full_join(sim_method_all, sim_method)
  
  sim_param <-  merge(sim_method_all, sim_param)
  
  #' provide a specific id to each parameter setting. This makes it 
  #' easier to process the results later on when there are repetitions
  sim_param$sim_param_id <- 1:nrow(sim_param)
}
