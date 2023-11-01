tabMain <- tabPanel("Main", 
                    withMathJax(),
                    sidebarLayout(
                      sidebarPanel(
                        p(strong("TODO: Title"),
                          p("L. Dijkstra, R. Foraita"),
                          em("To be Submitted")#,
                          #p(strong("DOI:"),"10.1002/PDS.4970"),
                        )
                      )
                      ,
                      mainPanel(
                        h1("Exposure Models for Electronic Healthcare Data"), 
                        p("This Shiny App contains all the confusion matrices 
                          for the exposure model simulation study. See for a detailed 
                          description our paper.")  ,
                        h4("Software"),
                        p("The R code used for this project is publicly available under the
                            GPL-3 license and can be found at: "), 
                        tags$ul(
                          tags$li(
                            a("expard", href="https://github.com/bips-hb/expard"),
                            ": an R package for simulating and fitting various exposure models"
                          ),
                          tags$li(
                            a("expard-simulation-study", href="https://github.com/bips-hb/expard-simulation-study"),
                            ": a GitHub repository containing all the code for the simulation (including this Shiny App)"
                          )
                        ),
                        
                        hr(),
                        
                        h4("Conflict of Interest"), 
                        
                        p("The authors declare that there are no conflicts of interest"),
                        
                        hr(), 
                        
                        h4("Contact"),
                        
                        p("Louis Dijkstra",
                          br(),
                          "Leibniz Institute for Prevention Research and Epidemiology - BIPS",
                          br(),
                          "Department Biometry & Data Management",
                          br(),
                          "E-mail:", 
                          a("dijkstra@leibniz-bips.de"),
                          br(),
                          a("http://www.leibniz-bips.de/en/")
                        )  
                      ) 
                      
                    )
)