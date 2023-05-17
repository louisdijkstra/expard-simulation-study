#' Plot estimates for long-term model
library(expard)
library(ggplot2)

# Generate the cohort
cohort <- expard::generate_cohort(n_patients = 1000, 
                                  simulation_time = 500, 
                                  n_drug_ADR_pairs = 1, 
                                  risk_model = "expard::risk_model_long_term(0.25, 50)",
                                  min_chance_drug = 0.05, 
                                  avg_duration = 5,
                                  prob_guaranteed_exposed = 1, 
                                  min_chance = 0.05, 
                                  max_chance = .9)

range <- dplyr::tibble(
  expand.grid(
    rate = seq(.01, 1, by = 0.01), 
    delay = seq(1, 80, by = 1)
  )
)

min_chance <- 0.05
max_chance <- 0.9

logit_min_chance <- log(min_chance / (1 - min_chance))
logit_max_chance <- log(max_chance / (1 - max_chance))

range$logl <- sapply(1:nrow(range), function(i) {
  cat(sprintf("%d/%d\n", i, nrow(range)))
  expard::loglikelihood_long_term(c(logit_min_chance, logit_max_chance, log(range$rate[i]), log(range$delay[i])), cohort[[1]]$drug_history, cohort[[1]]$adr_history)
})

readr::write_rds(range, "results/loglikelihood-long-term-model.rds")

range_interval <- range %>% filter(
  delay >= 30, 
  delay <= 60, 
  rate >= .1, 
  rate <= .5
)

r <- range %>% filter(logl == min(logl))
est_delay <- r$delay
est_rate <- r$rate

p <- ggplot(data = range_interval, aes(x = delay, y = rate, fill = logl)) +
  geom_tile() +
  geom_point(aes(x=50, y=0.25), colour="black") + 
  annotate("text", 
           x = 50,#true_value, 
           y = 0.28, # at certain percentage 
           label="true value",
           color = "black", 
           angle=0) + 
  geom_point(aes(x=est_delay, y=est_rate), colour="black") + 
  annotate("text", 
           x = est_delay,
           y = est_rate + .02, 
           label="estimate",
           color = "black", 
           angle=0) + 
  xlab("delay") +
  ylab("rate") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  ggtitle("Example of Log-likelihood 'Long Term' Risk Model") +
  scale_fill_gradient(low = "white", high = "blue", name = "log-likelihood") +
  theme_bw()

readr::write_rds(p, "figures/logl-longterm.rds")
p

