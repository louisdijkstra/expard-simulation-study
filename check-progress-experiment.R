#' Script for loading registry and checking whether there are any errors

#' Name of the repository
reg_name <- "analysis"

reg_dir <- sprintf("%s/registries/%s", getwd(), reg_name)

reg <- loadRegistry(file.dir = reg_dir, writeable = TRUE)

getStatus()
