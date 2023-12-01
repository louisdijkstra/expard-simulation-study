plot_confusion_matrix <- function(conf_matrix, 
                                  title = "",
                                  xlabel = "true model",
                                  ylabel = "selected model",
                                  add_legend = FALSE, 
                                  leave_out_zero_values = TRUE) { 
  
  melted_conf_matrix <- reshape2::melt(t(conf_matrix))
  
  true_model_labels <- colnames(conf_matrix)
  model_labels <- rownames(conf_matrix)
  
  melted_conf_matrix$character_value <- sapply(melted_conf_matrix$value, as.character)
  
  melted_conf_matrix$character_value[melted_conf_matrix$character_value == '0'] <- ''
  
  p <- ggplot(data = melted_conf_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(show.legend = add_legend) +
    geom_vline(xintercept = seq(0.5, 12, by = 1), color="gray", size=.25, alpha=.6) +  # set vertical lines between x groups
    geom_hline(yintercept = seq(0.5, 8, by = 1), color="gray", size=.25, alpha=.6) +  
    #theme_minimal() +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle(title) + 
    scale_x_discrete(expand=c(0,0), labels = true_model_labels) +
    scale_y_discrete(expand=c(0,0), labels = model_labels) +
    scale_fill_gradient(low = "white", high = "#1763aa", name = "percentage") +
    theme(panel.grid = element_line(color = "#8ccde3",
                                    size = 0.75,
                                    linetype = 2), 
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          panel.grid.minor = element_line(colour = "black", size = 5, linetype = 1) , 
          axis.text.y = element_text(angle=0),
          axis.text.x = element_text(angle=30, hjust=1,vjust=1),
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
    coord_fixed() 
  
  if (leave_out_zero_values) {
    p <- p + geom_text(aes(label = character_value))
  } else { 
    p <- p + geom_text(aes(label = value))
  }
  
  return(p) 
}