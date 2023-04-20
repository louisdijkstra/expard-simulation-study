library(batchtools)
library(CVN) 
library(CVNSim)
library(tidyverse)
library(hmeasure)

options(batchtools.verbose = TRUE)
options(stringsAsFactors = FALSE)

source("problems.R")
source("algorithms.R")
source("parameter-settings.R")

# GLOBAL PARAMETERS ------------------------------------------------------------

# Debugging --------------------------

#' For debugging. Only a limited number of parameter settings is used, see 
#' parameter-settings.R. Only 
test_run <- TRUE  

# Total number of replications for each parameter setting
if (test_run) { 
  repls <- 2 
} else { 
  repls <- 20
}

# Setting up the repository ---------

#' WARNING: If TRUE, removes the current repository and creates a new one. Used 
#' for debugging as well 
start_from_scratch <- TRUE 

#' Name of the repository
reg_name <- "expardsim"

#' Packages and files to load
packages = c("expard", "tidyverse", "hmeasure", "batchtools")
source = c("problems.R", "algorithms.R", "parameter-settings.R")

#' Number of concurrent jobs that run on the cluster (if the cluster is used)
max.concurrent.jobs <- 100


#' THE EXPERIMENT ITSELF -------------------------------------------------------

reg_dir <- sprintf("%s/registries/%s", getwd(), reg_name)

if (start_from_scratch) { # remove any previously existing registry and start a new one
  dir.create("registries", showWarnings = FALSE)
  unlink(reg_dir, recursive = TRUE)
  reg <- makeExperimentRegistry(file.dir = reg_dir, packages = packages, source = source)      
} else { 
  reg <- loadRegistry(file.dir = reg_dir, writeable = TRUE)
}

### add problems
addProblem(name = "sim_data", fun = simulator_wrapper, seed = 1) 

### add algorithms 
addAlgorithm(name = "expard", fun = expard_wrapper) 

### add the experiments

# parameters for the simulation
prob_design <- list(sim_data = sim_param)

# parameters for the methods
algo_design <- list(
  expard = algo_param
)

addExperiments(prob_design, algo_design, repls = repls)

### submit 
ids <- findNotStarted()
#submitJobs(ids = 1:4)
if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotStarted()
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
                              memory = 80000, walltime = 10*24*3600,
                              max.concurrent.jobs = max.concurrent.jobs))
} else {
  submitJobs(ids = ids)
}

waitForJobs()


#' COLLECT THE RESULTS --------------------------------------------------------- 
res <- reduceResultsList() 

# combine into one big data frame
res <- do.call(rbind.data.frame, res)

# combine the results with the parameters for the job
pars <- unwrap(getJobPars())

tab <- dplyr::left_join(res, pars)#, by = "job.id")

results <- list(
  truth = pars, 
  estimates = reduceResultsList() 
)

# store these results
readr::write_rds(results, "results/raw-results.rds", compress = "gz")

# post-process the results
source("process-results.R")

# TODO create the plots
#source("plots.R") 
