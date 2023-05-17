#' Algorithms used in the study 

expard_wrapper <- function(data, job, instance, ...) { 
  
  # fit all the models in the expard package
  res <- expard::fit_all_models(instance)
  
  #' add job identifiers which makes it easier to match 
  #' it with the simulation 
  res$repl <- job$repl
  res$job.id <- job$job.id
  
  return(res)
}

