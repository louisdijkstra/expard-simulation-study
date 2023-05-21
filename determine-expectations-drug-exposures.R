exp_no_patients_exposed <- 0.05
simulation_time <- 100

min_chance_drug <- 1 - (1 - exp_no_patients_exposed)^(1/simulation_time)
#' 0.0005128014
1 - (1 - min_chance_drug)^simulation_time

# Expected number of patients to suffer the ADR when not exposed at all
exp_patients <- 0.01
min_chance <- 1 - (1 - exp_patients)^(1/simulation_time)
min_chance
1e-4 


exp_patients <- 0.05
min_chance <- 1 - (1 - exp_patients)^(1/simulation_time)
min_chance
1e-4 
