library(expard)

#' @param job The batchtools job 
#' @param data data on the job
#'                     
simulator_wrapper <-
  function(job, data, 
           risk_model, past, rate, mu, sigma, delay,
           n_patients, simulation_time, 
           min_chance_drug, avg_duration, 
           prob_guaranteed_exposed, min_chance, max_chance, 
           sim_param_id 
           ) {
    
    cat(sprintf("The past is %d\n", past))
    # first determine the risk model 
    risk_model_fn <- switch(as.character(risk_model), 
                            "no-association" = "risk_model_no_association()", 
                            "current-use" = "risk_model_current_use()" , 
                            "past-use" = sprintf("risk_model_past(%d)", past) , 
                            "withdrawal" = sprintf("risk_model_withdrawal(%f)", rate) , 
                            "delayed" = sprintf("risk_model_delayed(%f, %f)", mu, sigma) , 
                            "decaying" = sprintf("risk_model_decaying(%f)", rate) , 
                            "delayed+decaying" = sprintf("risk_model_delayed_decaying(%f, %f, %f)", mu, sigma, rate) , 
                            "long-term" = sprintf("risk_model_long_term(%f, %f)", rate, delay))

    cohort <- generate_cohort(
      n_patients = n_patients,
      simulation_time = simulation_time,
      n_drug_ADR_pairs = 1,
      risk_model = risk_model_fn,
      min_chance_drug = min_chance_drug,
      avg_duration = avg_duration,
      prob_guaranteed_exposed = prob_guaranteed_exposed,
      min_chance = min_chance,
      max_chance = max_chance,
      verbose = FALSE
    )
    
    return(cohort[[1]])
  }