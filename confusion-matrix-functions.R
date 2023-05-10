compute_confusion_matrix <- function(scores, sim_id, 
                                     true_models = c("no_effect", "immediate", 
                                                     "withdrawal(rate = 0.25)", 
                                                     "withdrawal(rate = 0.5)", 
                                                     "withdrawal(rate = 1)", 
                                                     "increase_decrease(peak = 3)",
                                                     "increase_decrease(peak = 6)",
                                                     "long_time_after(rate = 0.1, delay = 100)",
                                                     "long_time_after(rate = 0.1, delay = 200)"), 
                                     models = c("no_effect", "immediate", "immediately_after", "extended", "long_time_after")) { 
  
  scores <- scores %>% dplyr::filter(simulation_id == sim_id)
  
  # initialize confusion matrix
  conf_matrix <- matrix(rep(0, length(models) * length(true_models)), ncol = length(true_models))
  colnames(conf_matrix) <- true_models
  rownames(conf_matrix) <- models
  
  for (i in 1:nrow(scores)) {
    model <- scores[i, "model"] %>% unlist()
    true_model <- scores[i, "true_model_with_parameters"] %>% unlist()
    conf_matrix[model, true_model] <- unlist(scores[i, "n"])
  }
  
  conf_matrix
}




#' @export
plot_confusion_matrix <- function(conf_matrix, 
                                  title = "",
                                  xlabel = "true model",
                                  ylabel = "selected model") { 
  
  melted_conf_matrix <- melt(t(conf_matrix))
  
  true_model_labels <- colnames(conf_matrix)
  model_labels <- rownames(conf_matrix)
  
  p <- ggplot(data = melted_conf_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    geom_vline(xintercept = seq(0.5, 12, by = 1), color="gray", size=.25, alpha=.6) +  # set vertical lines between x groups
    geom_hline(yintercept = seq(0.5, 8, by = 1), color="gray", size=.25, alpha=.6) +  
    #theme_minimal() +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle(title) + 
    scale_x_discrete(expand=c(0,0), labels = true_model_labels) +
    scale_y_discrete(expand=c(0,0), labels = model_labels) +
    scale_fill_gradient(low = "white", high = "red", name = "percentage") +
    theme(panel.grid = element_line(color = "#8ccde3",
                                    size = 0.75,
                                    linetype = 2), 
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          panel.grid.minor = element_line(colour = "black", size = 5, linetype = 1) , 
          axis.text.y = element_text(angle=45),
          axis.text.x = element_text(angle=45, hjust=1,vjust=1)) +
    coord_fixed() 
  
  return(p) 
}

plot_confusion_matrix(r[[1]], title = "hello")
