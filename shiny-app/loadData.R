# get all filenames for a directory
filenames_confusion_matrices <- list.files(path = "data/")

filenames_confusion_matrices <- str_split(filenames_confusion_matrices, "_")
parameter_settings <- as.data.frame(do.call(rbind, filenames_confusion_matrices))

parameter_settings <- parameter_settings %>% select(-V1, -V2, -V6)

colnames(parameter_settings) <- c('N', 'T', 'prob_exposed', 'min_chance', 'max_chance')

parameter_settings$filename <- paste0('data/', list.files(path = "data/"))

parameter_settings$max_chance <- sapply(parameter_settings$max_chance, function(x) as.numeric(str_split(x, ".rds")[[1]][1]))

parameter_settings$id <- 1:nrow(parameter_settings)

plots <- lapply(parameter_settings$filename, function(filename) {
  readr::read_rds(filename)
})

parameter_settings$plots <- plots
