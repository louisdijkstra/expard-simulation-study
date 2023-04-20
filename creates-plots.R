###############################
# plot.R 
#
# Contains all functions for 
# plotting the results 
###############################

require(ggplot2)
require(reshape2)

#' Plot of the Confusion Matrix 
#' 
#' Returns a plot of the confusion matrix 
plot_confusion_matrix <- function(conf_matrix, 
                                  title = "",
                                  xlabel = "actual model",
                                  ylabel = "predicted model",
                                  true_models = c("no_effect", "immediate", 
                                                  "withdrawal(rate = 0.25)", 
                                                  "withdrawal(rate = 0.5)", 
                                                  "withdrawal(rate = 1)", 
                                                  "increase_decrease(peak = 3)",
                                                  "increase_decrease(peak = 6)",
                                                  "long_time_after(rate = 0.1, delay = 100)",
                                                  "long_time_after(rate = 0.1, delay = 200)"), 
                                  models = c("no_effect", "immediate", "immediately_after", "extended", "long_time_after"),
                                model_labels = c("no effect", "immediate", "immediately after", "extended", "long time after")) { 

  melted_conf_matrix <- reshape2::melt(t(conf_matrix))
  
  true_model_labels=c('no_effect' = 'no effect',
           'immediate' = 'immediate',
           "withdrawal(rate = 0.25)" = parse(text = TeX("$withdrawal(\\gamma = 0.25)$")),
           "withdrawal(rate = 0.5)" = parse(text = TeX("$withdrawal(\\gamma = 0.5)$")),
           "withdrawal(rate = 1)" = parse(text = TeX("$withdrawal(\\gamma = 1)$")),
           "increase_decrease(peak = 3)" = "delayed(k = 3)", 
           "increase_decrease(peak = 6)" = "delayed(k = 6)", 
           "long_time_after(rate = 0.1, delay = 100)" = parse(text = TeX("long time after($\\gamma = 0.1$, k = 100)")),
             "long_time_after(rate = 0.1, delay = 200)" = parse(text = TeX("long time after($\\gamma = 0.1$, k = 200)")))
  
  
  p <- ggplot(data = melted_conf_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    theme_minimal() +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle(title) + 
    scale_x_discrete(expand=c(0,0), labels = true_model_labels) +
    scale_y_discrete(expand=c(0,0), labels = model_labels) +
    scale_fill_gradient(low = "white", high = "red", name = "percentage") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
          panel.grid.minor = element_line(colour = "black", size = 5) , 
          axis.text.y = element_text(angle=20),
          axis.text.x = element_text(angle=60, hjust=0.95,vjust=0.9)) + 
    coord_fixed() 

  return(p)
}

