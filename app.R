################################################################################
#     Sample Size Determination for Multivariate Cluster Randomised Trial      #
################################################################################

# Libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(shiny)
library(shinythemes)


# Load data sets
results_iu <- readRDS("data/results_iu.RDS")
results_omni <- readRDS("data/results_omni.RDS")
results_homog <- readRDS("data/results_homog.RDS")

results_iu_plot <- readRDS("data/data_iu_pl.RDS")
results_omni_plot <- readRDS("data/data_omni_pl.RDS")
results_homog_plot <- readRDS("data/data_homog_pl.RDS")

# Names of columns 
names_columns <- c("Cluster size" = "n1", "Number of clusters" = "n2")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                "#D55E00", "#CC79A7", "#661100")


# Define UI =============================================================
ui <- fluidPage(
    
    # Application title
    titlePanel("Sample Size Determination for Bayesian Hypothesis Testing in
               Cluster Randomised Trial"),
    
    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel("Results",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("test",
                                     "Type of test:",
                                     choices = list("Intersection-union" = "iu",
                                                    "Omnibus" = "omnibus",
                                                    "Homogeneity of treatment effects" = "homogeneity")),
                         sliderInput("n1",
                                     "Clsuter size",
                                     choices = list("5" = 5, "15" = 15, "30" = 30)),
                         selectInput("rho0",
                                     "Outcome-specific ICC for outcome 1",
                                     choices = list("0.01" = 0.01, "0.05" = 0.05)
                         ),
                         selectInput("rho1",
                                     "Intersubject between-outcome ICC",
                                     choices = list("0.005" = 0.005, "0.025" = 0.025)
                         ),
                         selectInput("rho2",
                                     "Intrasubject between-outcome ICC",
                                     choices = list("0.02" = 0.02, "0.05" = 0.05)
                         ),
                         selectInput("pmp",
                                     "Posterior Model Probability threshold",
                                     choices = list(0.9, 0.95)),
                         conditionalPanel(
                             condition = "input.test == 'iu'",
                             sliderInput("outcome1",
                                         "Treatment effect in outcome 1",
                                         choices = list("0.3" = 0.3, "0.5" = 0.5, "0.7" = 0.7)
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.3'",
                                 selectInput(
                                     "Treatment effect in outcome 2",
                                     choices = list("0.5" = 0.5, "0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.5'",
                                 selectInput(
                                     "Treatment effect in outcome 2",
                                     choices = list("0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.7'",
                                 selectInput(
                                     "Treatment effect in outcome 2",
                                     choices = list("0.9" = 0.9))
                             ),
                             
                             
                         ),
                         conditionalPanel(
                             condition = "input.test == 'omnibus'",
                             sliderInput("outcome1",
                                         "Treatment effect in outcome 1", 
                                         choices = list("0.2" = 0.2, "0.3" = 0.3, "0.5" = 0.5, "0.7" = 0.7)
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.2'",
                                 selectInput(
                                     "Treatment effect in outcome 2",
                                     choices = list("0.3" = 0.3, "0.5" = 0.5, "0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.3'",
                                 selectInput(
                                     "Treatment effect in outcome 2",
                                     choices = list("0.5" = 0.5, "0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.5'",
                                 selectInput(
                                     "Treatment effect in outcome 2",
                                     choices = list("0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.7'",
                                 selectInput(
                                     "Treatment effect in outcome 2",
                                     choices = list("0.9" = 0.9))
                             )),
                         conditionalPanel(condition = "input.test == 'homogeneity'",
                                          radioButtons("outcome1",
                                                       "Treatment effects (outcome 1 and outcome 2",
                                                       choices = list("0.3 and 0.2" = 0.3,
                                                                      "0.6 and 0.5" = 0.6,
                                                                      "0.9 and 0.8" = 0.9)),
                                          selectInput("delta", "Difference",
                                                      choices = list("0.2" = 0.2, "0.3" = 0.3,))),
                         
                         conditionalPanel(
                             condition = "input.test == 'iu'",
                             checkboxGroupInput("hypothesis",
                                                "True hypothesis in plot:",
                                                c("H1" = 1,
                                                  "H2" = 2,
                                                  "H3" = 3,
                                                  "H4" = 4), 
                                                selected = 1
                             )
                         ),
                         conditionalPanel(
                             condition = "input.test == 'omnibus'",
                             checkboxGroupInput("hypothesis",
                                                "True hypothesis in plot:",
                                                c("H1" = 1,
                                                  "H2" = 2,
                                                  "H3" = 3,
                                                  "H4" = 4), 
                                                selected = 1
                             )
                         ),
                         conditionalPanel(
                             condition = "input.test == 'homogeneity'",
                             checkboxGroupInput("hypothesis",
                                                "True hypothesis in plot:",
                                                c("H1" = 1,
                                                  "H2" = 2), 
                                                selected = 1
                             )),
                         checkboxInput("incl_bayes",
                                       label = "Include Bayes factor",
                                       value = TRUE),
                         
                         actionButton("go", "Determine", class = "btn btn-primary")
                     ),
                     # Show a plot of the generated distribution
                     mainPanel(
                         # Plot with PMP 
                         plotOutput("distPlotPMP"),
                         # Tables
                         h3("Final Sample Size"),
                         ## Table with PMP
                         tableOutput("tablePMP"),
                         # Plot with BF
                         plotOutput("distPlotBF"),
                         # Text
                         uiOutput("interpretation")
                     )
                     
                 ),
                 tabPanel("Information",
                          #TODO: Change this with the correct reference
                          p(HTML("This Shiny app display the result from the research 
                                <i><a href='https://www.overleaf.com/project/65f16c0441556b4e4487bc30' 
                        target='_blank'>Method for Sample Size Determination for Cluster Randomized Trials Using the Bayes Factor</a></i> by Barragan et al. (2024).  
                                The source code is available at <i><a 
                                href='https://github.com/cnbi/Bayesian-Sample-Size-Determination-CRT' 
                        target='_blank'>Bayesian Sample Size Determination-CRT</a></i>")),
                          p("To reference the application, please use the following:"),
                          p(HTML("@misc{barragan_sample_2024,
	title = {Sample size determination for cluster randomised trials with the Bayes factor}, <br>
	shorttitle = {Bayes sample size determination: CRT}, <br>
	url = {https://utrecht-university.shinyapps.io/BayesSamplSizeDet-CRT/}, <br>
	publisher = {Utrecht University}, <br>
	author = {Barragan, Camila and Moerbeek, Mirjam and Hoijtink, Herbert}, <br>
	month = apr, <br>
	year = {2024}, <br>
}
                        ")),
                          p("Or in APA style:"),
                          p(HTML("Barragan, C., Moerbeek, M., & Hoijtink, H. (2024). 
                          <i>Sample size determination for cluster randomised trials 
                          with the Bayes factor </i> [Shiny app]. Utrecht University. 
                          https://utrecht-university.shinyapps.io/BayesSamplSizeDet-CRT/")
                          ),
                          p("For any bug, error, or feedback you may contact Camila Barragán via email at cn.barragan.ibanez@gmail.com or GitHub")
                 )
        )
    )
)

# Server ========================================================
server <- function(input, output) {
    
    # Select dataset
    dataset <- eventReactive(input$go, {
        dataset_tb <- switch(input$test,
                             iu = results_iu,
                             omnibus = results_omni,
                             homgeneity = results_homog)
        
        # Filter dataset based on ICCs, n1, and threshold
        filtered_data <- dataset_tb() %>% filter(n1 == as.numeric(input$n1), 
                                                 rho0 == as.numeric(input$rho0),
                                                 rho1 == as.numeric(input$rho1),
                                                 rho2 == as.numeric(input$rho2),
                                                 pmp == as.numeric(input$pmp))
        
        
        # Filter effect sizes and delta
        second_filter <- reactive({
            if (input$test == "iu") {
                filtered <- filtered_data() %>% filter(eff_size1 == as.numeric(input$outcome1),
                                                       eff_size2 == as.numeric(input$outcome2))
            } else if (input$test == "omnibus") {
                filtered <- filtered_data() %>% filter(
                    eff_size1 == as.numeric(input$outcome1),
                    eff_size2 == as.numeric(input$outcome2)
                )} else {
                    filtered <- filtered_data %>% filter(
                        eff_size1 == as.numeric(input$outcome1),
                        delta == as.numeric(input$delta)
                    )
                }
        }
        )
        return(second_filter)
    })
    
    # Give format to table and render
    hypotheses <- c("H1", "H2", "H3", "H4")
    
    output$tablePMP <- renderTable({
        if (input$test == "iu") {
            data1 <- dataset() %>% 
                select(all_of(c("eta.PMP1", "eta.PMP2", "eta.PMP3", "eta.PMP4"))) %>% 
                pivot_longer(cols = everything(),
                             values_to = "P(PMP.H > threshold)") %>% 
                mutate(Hypothesis = hypotheses, .before = 1)
            data2 <- dataset() %>% 
                select(all_of(c("mean.PMP1", "mean.PMP2", "mean.PMP3", "mean.PMP4"))) %>% 
                pivot_longer(cols = everything(), values_to = "Error") %>% 
                mutate(Error = 1 - Error)
            bind_cols(data1, data2)
        } else if (input$test == "omnibus") {
            data1 <- dataset() %>% 
                select(all_of(c("eta.PMP1", "eta.PMP2", "eta.PMP3", "eta.PMP4"))) %>% 
                pivot_longer(cols = everything(),
                             values_to = "P(PMP.H > threshold)") %>% 
                mutate(Hypothesis = hypotheses, .before = 1)
            data2 <- dataset() %>% 
                select(all_of(c("mean.PMP1", "mean.PMP2", "mean.PMP3", "mean.PMP4"))) %>% 
                pivot_longer(cols = everything(), values_to = "Error") %>% 
                mutate(Error = 1 - Error)
            bind_cols(data1, data2)
        } else {
            data1 <- dataset() %>% 
                select(all_of(c("eta.PMP1", "eta.PMP2"))) %>% 
                pivot_longer(cols = everything(),
                             values_to = "P(PMP.H > threshold)") %>% 
                mutate(Hypothesis = hypotheses[1:2], .before = 1)
            data2 <- dataset() %>% 
                select(all_of(c("mean.PMP1", "mean.PMP2"))) %>% 
                pivot_longer(cols = everything(), values_to = "Error") %>% 
                mutate(Error = 1 - Error)
            bind_cols(data1, data2)
        }},
        
        # Make pretty table
        striped = TRUE, spacing = "l", digits = 3, width = "90%", align = "c"
    )
    
    
    
    
    # Make plot
    ## Select dataset and filter data
    dataset_pl <- eventReactive(input$go, {
        dataset_ <- switch(input$test,
                           iu = results_iu_plot,
                           omnibus = results_omni_plot,
                           homgeneity = results_homog_plot)
        
        # Filter dataset based on ICCs, n1, and threshold
        filtered_data <- dataset_() %>% filter(n1 == as.numeric(input$n1), 
                                               rho0 == as.numeric(input$rho0),
                                               rho1 == as.numeric(input$rho1),
                                               rho2 == as.numeric(input$rho2),
                                               pmp == as.numeric(input$pmp))
        
        
        # Filter effect sizes and delta
        second_filter <- reactive({
            if (input$test == "iu") {
                filtered <- filtered_data() %>% filter(eff_size1 == as.numeric(input$outcome1),
                                                       eff_size2 == as.numeric(input$outcome2))
            } else if (input$test == "omnibus") {
                filtered <- filtered_data() %>% filter(
                    eff_size1 == as.numeric(input$outcome1),
                    eff_size2 == as.numeric(input$outcome2)
                )} else {
                    filtered <- filtered_data %>% filter(
                        eff_size1 == as.numeric(input$outcome1),
                        delta == as.numeric(input$delta)
                    )
                }
        }
        )
        reactive_data_plot <- reactive({
            data_plot_filt <- second_filter %>% filter(hypothesis %in% input$hypothesis)
            data_plot_filt
        })
        return(reactive_data_plot)
    })
    
    # Make 
    output$distPlotPMP <- renderPlot({
        # Draw plot PMP
        plot_pmp <- ggplot(dataset_pl(), aes(x = PMP, color = as.factor(hypothesis), fill = as.factor(hypothesis))) + 
            geom_histogram(binwidth = 1, aes(y = after_stat(density)), alpha = 0.5, position = "identity") +
            geom_density(alpha = .2) +
            geom_vline(aes(xintercept = log(pmp_thresh), colour = cbbPalette[9]), linetype = "dashed") +
            scale_fill_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8]), name = "Hypothesis") +
            scale_color_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8]), name = "Hypothesis") +
            ylab("Density") + xlab("Posterior Model Probability") +
            theme(legend.position = "bottom", axis.title = element_text(size = 16),
                  axis.text = element_text(size = 15),
                  plot.caption = element_text(hjust = 0, size = 12),
                  plot.title = element_text(size = 14, face = "bold")) + 
            labs(title = "Posterior Model Probabilities favouring true hypothesis")
        
    })
    
    if (input$incl_bayes == TRUE) {
    output$distPlotBF <- renderPlot({
        # Draw plot BF
        
        plot_BF <- ggplot(dataset_pl(), aes(x = log(BF), color = as.factor(hypothesis), fill = as.factor(hypothesis))) + 
            geom_histogram(binwidth = 1, aes(y = after_stat(density)), alpha = 0.5, position = "identity") +
            geom_density(alpha = .2) +
            geom_vline(aes(xintercept = log(pmp_thresh), colour = cbbPalette[9]), linetype = "dashed") +
            scale_fill_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8]), name = "Hypothesis") +
            scale_color_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8]), name = "Hypothesis") +
            ylab("Density") + xlab(bquote("log Bayes factor"["mu"])) +
            theme(legend.position = "bottom", axis.title = element_text(size = 16),
                  axis.text = element_text(size = 15),
                  plot.caption = element_text(hjust = 0, size = 12),
                  plot.title = element_text(size = 14, face = "bold")) + 
            labs(title = "Bayes factor favouring true hypothesis")
    })
    }
    
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
