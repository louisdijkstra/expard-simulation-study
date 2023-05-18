source("parameter-settings.R")

repl <- 20
n_cores <- 200
n_param <- nrow(sim_param)

# N = 100, T = 100

hours <- (144/2 * 223.319 * repl) / 3600
days <- hours / 24

hours
days

hours / n_cores
days / n_cores

# N = 1000, T = 100

hours <- (144/2 * 2815.273 * repl) / 3600
days <- hours / 24

hours
days

hours / n_cores
days / n_cores

