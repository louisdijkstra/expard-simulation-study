#' All function for plotting

#' @export
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




#' Plots for showing the results for the estimates
#' Example: plot_parameter_estimate_past(est[[5]], true_value = truth[5, past])
plot_parameter_estimate_past <- function(estimate, 
                                         true_value = 10, 
                                         title = "Example of Log-likelihood 'Paste Use' Risk Model",
                                         xlabel = "parameter of the 'past use' risk model", 
                                         ylabel = "log-likelihood") { 
  ggplot(data = estimate %>% filter(model == "past-use")) + 
    geom_vline(xintercept = true_value, color="red", size=1, alpha=.7, linetype="dotted") +
    annotate("text", 
             x = true_value - .3,#true_value, 
             y = (max(estimate$loglikelihood) - min(estimate$loglikelihood))*.5 + min(estimate$loglikelihood), # at certain percentage 
             label="true value",
             color = "red", 
             angle=90) + 
    geom_point(aes(x = past, y = loglikelihood)) + 
    ggtitle(title) + 
    xlab(xlabel) + 
    ylab(ylabel) + 
    theme_bw()
}



#' plot true risk model and estimated risk model 
#' Example: 
#' plot_estimated_true_risk_models(estimated_risk_model = risk_model_delayed(80, 30))
plot_estimated_true_risk_models <- function(drug_history = c(rep(0, 4), rep(1, 6), rep(0, 90)),
                                            true_risk_model = risk_model_long_term(0.25, 50),
                                            estimated_risk_model = risk_model_current_use(),
                                            title = "", 
                                            ylim = c(0,1), 
                                            shaded_area = TRUE, 
                                            fill = "black", 
                                            alpha = 0.3) {
  
  p <- expard::plot_risk(drug_history = drug_history, 
                         risk_model = true_risk_model,
                         title = title, 
                         ylim = ylim, 
                         shaded_area = shaded_area,
                         fill = fill, 
                         alpha = alpha)
  
  # determine the risks given the drug prescription history 
  # and the risk model given by risk_model
  risks <- estimated_risk_model(drug_history)
  
  # create a dataset with the time points, the drug prescriptions 
  # and the risks
  data <- data.frame(
    t = 1:length(drug_history), 
    drug = drug_history,
    risk = risks
  )
  
  p <- p + geom_point(data = data, mapping = aes(x = t, y = risks), colour = "black", shape=1)
  
  return(p)
}



