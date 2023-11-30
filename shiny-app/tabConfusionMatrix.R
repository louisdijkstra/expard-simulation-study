tabConfusionMatrix <- tabPanel("Confusion Matrix",
                     withMathJax(),
                     sidebarLayout(
                       sidebarPanel(
                         
                         # Select the probability exposed  -----
                         sliderTextInput(
                           inputId = "prob_exposed",
                           label = "Probability to be exposed (\\(\\mu_{E}\\))", 
                           choices = parameter_settings$prob_exposed %>% unique() %>% sort(),
                           grid = TRUE
                         ),
                         
                         # Select the probability exposed  -----
                         sliderTextInput(
                           inputId = "min_chance",
                           label = "\\(\\pi_0\\)", 
                           choices = parameter_settings$min_chance %>% unique() %>% sort(),
                           grid = TRUE
                         ),
                         
                         # Select the probability exposed  -----
                         sliderTextInput(
                           inputId = "max_chance",
                           label = "\\(\\pi_1\\)", 
                           choices = parameter_settings$max_chance %>% unique() %>% sort(),
                           grid = TRUE
                         ),
                        
                         useShinyjs()
                       ),
                       mainPanel(
                         plotOutput("confusionMatrixPlot", height = 700)
                       ))
                     
)
