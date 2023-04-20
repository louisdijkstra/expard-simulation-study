# create all figures  

library(readr)
library(dplyr)
library(ggplot2)

# load data
data <- readr::read_rds("results/best-scores.rds")

source("parameter-settings.R")

sim_param
algo_param
settings <- merge(sim_param, algo_param)

get_subset <- function(data, row_settings) { 
  data %>% filter(
    sim_param_id == row_settings$sim_param_id, 
    type_weight_matrix == row_settings$type_weight_matrix
  )
}

b = get_subset(data, settings[1,])

create_boxplot <- function(data, row_settings, var = c("F1", "F2"), 
                           xlabel = "Selection method", title = NULL) { 
  
  b <- get_subset(data, row_settings)
  
  if (is.null(title)) { 
    if (row_settings$type == "scale-free") { 
      graph_desc <- sprintf("(Scale-free, power coeff. %g)", row_settings$power)
    } else { 
      graph_desc <- sprintf("(Erdos-Rényi, prob. %g)", row_settings$probability)
    }
    title <- sprintf("p = %d, n = %d, weight matrix: %s %s", 
                     row_settings$p, 
                     row_settings$n, 
                     row_settings$type_weight_matrix, 
                     graph_desc)
    
    title <- sprintf("p = %d, n = %d, weight matrix: %s %s", 
                     row_settings$p, row_settings$n_obs, 
                     row_settings$type_weight_matrix, 
                     graph_desc) 
                  
  }
  
  ggplot(b, aes(x = score, y = get(var[1]), color = score)) + 
    geom_boxplot() + 
    xlab(xlabel) + 
    ggtitle(title) + 
    ylab(var) + 
    scale_color_brewer(palette="Dark2") + 
    theme_classic()
}

for (i in 1:nrow(settings)) { 
  row_settings <- settings[i, ]
  
  p <- create_boxplot(data, row_settings, var = "F1")  

  filename <- paste0(lapply(as.list(row_settings), function(x) as.character(x)), collapse = "_")
  
  filename <- paste0("figures/", filename, collapse = "")
  
  filename <- paste0(filename, ".pdf", collapse = "")
  
  ggplot2::ggsave(
    filename,
    p,
    device = NULL,
    path = NULL,
    scale = 1,
    width = 7,
    height = 4,
    units = c("in", "cm", "mm", "px"),
    dpi = 300,
    limitsize = TRUE,
    bg = NULL
  )
}
create_boxplot(data, settings[3,])

