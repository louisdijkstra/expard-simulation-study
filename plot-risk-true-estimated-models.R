# plot true risk model and estimated risk model 

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

plot_estimated_true_risk_models(estimated_risk_model = risk_model_delayed(80, 30))
  