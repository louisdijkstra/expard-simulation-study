only_sim_param <- dplyr::as_tibble(
  expand.grid(
    n_patients = c(1000),
    simulation_time = c(100),  
    prob_exposed = c(0.01, 0.1,.5), 
    avg_duration = c(5), 
    min_chance = c(1e-4, 1e-3), 
    max_chance = c(.01,.1,.2,.3)
  )
)