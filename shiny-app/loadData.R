confusion_matrices <- readr::read_rds("data/confusion-matrices.rds")

only_sim_param$id <- 1:nrow(only_sim_param)

only_sim_param$title <- lapply(1:nrow(only_sim_param), function(i) { 
  n_patients <- only_sim_param$n_patients[i]
  simulation_time <- only_sim_param$simulation_time[i] 
  avg_duration <- only_sim_param$avg_duration[i] 
  prob_exposed <- only_sim_param$prob_exposed[i]
  min_chance <- only_sim_param$min_chance[i]
  max_chance <- only_sim_param$max_chance[i]
  
  TeX(sprintf("Probability exposed = $%g$, $\\pi_1 = %g$, $\\pi_0 = %g$", prob_exposed,
                       max_chance, min_chance))
})

only_sim_param <- only_sim_param %>% dplyr::rename(
  N = n_patients,
  T = simulation_time
)

plots <- lapply(1:length(confusion_matrices), function(i) {
  plot_confusion_matrix(confusion_matrices[i][[1]], title = only_sim_param$title[i][[1]])
})

parameter_settings <- only_sim_param
parameter_settings$plots <- plots
