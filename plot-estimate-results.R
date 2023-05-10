#' Plots for showing the results for the estimates
plot_parameter_estimate_past <- function(estimate, 
                                         true_value = 10, 
                                         title = "Example of Log-likelihood 'Paste Use' Risk Model",
                                         xlabel = "parameter of the 'past use' risk model", 
                                         ylabel = "log-likelihood") { 
  ggplot(data = estimate %>% filter(model == "past-use")) + 
    geom_vline(xintercept = true_value, color="red", size=1, alpha=.7, linetype="dotted") +
    geom_point(aes(x = past, y = loglikelihood)) + 
    ggtitle(title) + 
    xlab(xlabel) + 
    ylab(ylabel) + 
    theme_bw()
}

plot_parameter_estimate_past(est[[5]], true_value = truth[5, past])
