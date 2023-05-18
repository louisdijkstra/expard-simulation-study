#####################################
# plot-all-risk-models.R 
#
# Script for plotting the various 
# risk models implemented in this 
# package. See more detail, 
# R/plot-risks.R and R/risk-models.R
# in the expard packages
#
# The resulting plots are stored in 
# the figures/ folder
#####################################
library(expard)

# the drug history used for the plots
drug_history <- c(rep(0,4), rep(1,6), rep(0, 10))

# a longer history used for effect models that take a long time
long_drug_history <- c(rep(0,4), rep(1,6), rep(0, 70))

# dimensions of the plots 
width  <- 6
height <- 3

# No effect model ---------------------
plot_risk(drug_history, 
          risk_model = risk_model_no_association(),
          title = "no assocation")
ggsave("figures/risk_model_no_association.pdf", width = width, height = height)

# Current use model ---------------------
plot_risk(drug_history, 
          risk_model = risk_model_current_use(),
          title = "current use")
ggsave("figures/risk_model_current_use.pdf", width = width, height = height)

# Past use model ---------------------
plot_risk(long_drug_history, 
          risk_model = risk_model_past(5),
          title = "past use (delay = 5)")
ggsave("figures/risk_model_past_use_delay5.pdf", width = width, height = height)

plot_risk(long_drug_history, 
          risk_model = risk_model_past(10),
          title = "past use (delay = 10)")
ggsave("figures/risk_model_past_use_delay10.pdf", width = width, height = height)

# Withdrawal model --------------------
rate <- c(.5,1)

sapply(rate, function(rate) { 
  p <- plot_risk(drug_history, risk_model = risk_model_withdrawal(rate), 
                 title = sprintf("withdrawal (rate = %g)", rate))
  ggsave(sprintf("figures/risk_withdrawal_%g.pdf", rate), p, width = width, height = height)
})

# Delayed model ----------------

delay <- c(2, 5) 
std <- 2
sapply(delay, function(delay) { 
  p <- plot_risk(drug_history, risk_model = risk_model_delayed(delay, std), 
                 title = sprintf("delayed (delay = %g, std = %g)", delay, std))
  ggsave(sprintf("figures/risk_model_delayed_%g_%g.pdf", delay, std), p, width = width, height = height)
})


# Decaying model ----------------
rate <- c(.5,1)

sapply(rate, function(rate) { 
  p <- plot_risk(drug_history, risk_model = risk_model_decaying(rate), 
                 title = sprintf("decaying (rate = %g)", rate))
  ggsave(sprintf("figures/risk_decaying_%g.pdf", rate), p, width = width, height = height)
})

# Delayed + decaying model -----
p <- plot_risk(drug_history, risk_model = risk_model_delayed_decaying(10, 2, 0.5), 
               title = sprintf("delayed + decaying (delay = 10, std = 2, rate = 0.5)"))
ggsave(sprintf("figures/risk_delayed+decaying.pdf"), p, width = width, height = height)


# Long term model ---------------
rate <- 0.25
delay <- 50

p <- plot_risk(long_drug_history, risk_model = risk_model_long_term(rate = rate, delay = delay))
ggsave(sprintf("figures/risk_long_term_%g_%g.pdf", rate, delay), p, width = width, height = height)
