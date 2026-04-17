################################################################################
#     Sample Size Determination for Multivariate Cluster Randomised Trial      #
################################################################################

# Libraries
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(shinycssloaders)


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
    theme = shinytheme("flatly"),
    # Application title
    titlePanel("Sample Size Determination for Bayesian Hypothesis Testing in
               Multivariate Cluster Randomised Trials"),
    
    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel("Results",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         selectInput("test",
                                     "Type of test",
                                     choices = list("Intersection-union" = "iu",
                                                    "Omnibus" = "omnibus",
                                                    "Homogeneity of treatment effects" = "homogeneity")),
                         selectInput("n1",
                                     "Cluster size",
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
                                     choices = list("0.2" = 0.2, "0.5" = 0.5)
                         ),
                         selectInput("pmp_thres",
                                     "Posterior Model Probability threshold",
                                     choices = list(0.9, 0.95)
                         ),
                         conditionalPanel(
                             condition = "input.test == 'iu'",
                             selectInput("outcome1",
                                         "Treatment effect in outcome 1",
                                         choices = list("0.3" = 0.3, "0.5" = 0.5, "0.7" = 0.7)
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.3'",
                                 selectInput("outcome2",
                                             "Treatment effect in outcome 2",
                                             choices = list("0.5" = 0.5, "0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.5'",
                                 selectInput("outcome2",
                                             "Treatment effect in outcome 2",
                                             choices = list("0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.7'",
                                 selectInput("outcome2",
                                             "Treatment effect in outcome 2",
                                             choices = list("0.9" = 0.9))
                             )
                         ),
                         conditionalPanel(
                             condition = "input.test == 'omnibus'",
                             selectInput("outcome1",
                                         "Treatment effect in outcome 1", 
                                         choices = list("0.2" = 0.2, "0.3" = 0.3, "0.5" = 0.5, "0.7" = 0.7)
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.2'",
                                 selectInput("outcome2",
                                             "Treatment effect in outcome 2",
                                             choices = list("0.3" = 0.3, "0.5" = 0.5, "0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.3'",
                                 selectInput("outcome2",
                                             "Treatment effect in outcome 2",
                                             choices = list("0.5" = 0.5, "0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.5'",
                                 selectInput("outcome2",
                                             "Treatment effect in outcome 2",
                                             choices = list("0.7" = 0.7, "0.9" = 0.9))
                             ),
                             conditionalPanel(
                                 condition = "input.outcome1 == '0.7'",
                                 selectInput("outcome2",
                                             "Treatment effect in outcome 2",
                                             choices = list("0.9" = 0.9))
                             )),
                         conditionalPanel(condition = "input.test == 'homogeneity'",
                                          radioButtons("outcome1",
                                                       "Treatment effects (outcome 1 and outcome 2)",
                                                       choices = list("0.3 and 0.2" = 0.3,
                                                                      "0.6 and 0.5" = 0.6,
                                                                      "0.9 and 0.8" = 0.9)),
                                          selectInput("delta", "Difference",
                                                      choices = list("0.2" = 0.2, "0.3" = 0.3))
                         ),
                         conditionalPanel(
                             condition = "input.test != 'homogeneity'",
                             checkboxGroupInput("hypothesis",
                                                "True hypothesis in plot",
                                                c("H1" = 1,
                                                  "H2" = 2,
                                                  "H3" = 3,
                                                  "H4" = 4), 
                                                selected = 1)
                         ),
                         conditionalPanel(
                             condition = "input.test == 'homogeneity'",
                             checkboxGroupInput("hypothesis",
                                                "True hypothesis in plot:",
                                                c("H1" = 1,
                                                  "H2" = 2), 
                                                selected = 1)
                         ),
                         checkboxInput("incl_bayes",
                                       label = "Include Bayes factor plot",
                                       value = TRUE),
                         
                         actionButton("go", "Determine Sample Size", class = "btn-primary btn-block")
                     ),
                     # Show a plot of the generated distribution
                     mainPanel(
                         
                         # Plot with PMP
                         shinycssloaders::withSpinner(
                             plotOutput("distPlotPMP"),
                             type = 6),
                         # Tables
                         h3("Final Sample Size"),
                         ## Table with PMP
                         shinycssloaders::withSpinner(
                             tableOutput("tablePMP"), type = 6),
                         # Text
                         wellPanel(
                             style = "background-color: #f0f7fb; border-left: 6px solid #2e6da4; padding: 20px;",
                             h4("Interpretation", style = "margin-top: 0; color: #2e6da4; font-weight: bold;"),
                             shinycssloaders::withSpinner(
                                 uiOutput("interpretation"), 
                                 type = 6
                             )
                         ),
                         # Plot with BF
                         conditionalPanel(
                             condition = "input.incl_bayes == true",
                             h3("Bayes Factor Distribution"),
                             shinycssloaders::withSpinner(
                                 plotOutput("distPlotBF"), type = 6)
                         )
                     )
                 )
        ),
        tabPanel("Information",
                 fluidRow(
                     column(10, offset = 1,
                            br(),
                            wellPanel(
                                style = "background-color: #ffffff; border-top: 5px solid #2c3e50; box-shadow: 2px 2px 10px #f0f0f0;",
                                h3(icon("book-open"), "Research Background", style = "margin-top: 0; color: #2c3e50;"),
                                p("This Shiny app display the result from the research: "),
                                tags$ul(style = "list-style-type: none; padding-left: 10px;",
                                        tags$li(icon("file-alt"), tags$i(tags$a(
                                            href = "https://www.overleaf.com/project/65f16c0441556b4e4487bc30",
                                            target = "_blank", "Method for Sample Size Determination for Cluster 
                                            Randomized Trials Using the Bayes Factor")),
                                            " by Barragan  and Moerbeek."),
                                        br(),
                                        tags$li(icon("layer-group"), "Further details can be found in the OSF project: ",
                                                tags$i(tags$a(href = "https://osf.io/g5ydk",
                                                              target = "_blank", 
                                                              "Sample Size Determination for Cluster Randomised 
                                                              Trials with Multiple Outcomes: 
                                                                  A Bayesian Hypothesis Testing Approach"))),
                                        br(),
                                        tags$li(icon("code"), "The source code is available at ",
                                                tags$i(tags$a(href = "https://github.com/cnbi/SSD-Multivariate-CRT",
                                                              target = "_blank",
                                                              "Bayesian Sample Size Determination in Multivariate 
                                                              Cluster Randomised Trials")))
                                )
                            ),
                            wellPanel(
                                #TODO: Change this with the correct reference
                                style = "background-color: #fcfcfc; border-left: 4px solid #d1d1d1;",
                                h4(icon("quote-right"), "How to reference",style = "color: #2c3e50;"),
                                hr(),
                                p(strong("BibTex")),
                                tags$pre(style = "background-color: #f4f4f4; padding: 15px; border: 1px solid #ddd;",
                                         HTML("@misc{barragan_sample_2024,
	title = {Sample size determination for cluster randomised trials with the Bayes factor}, 
	shorttitle = {Bayes sample size determination: CRT}, 
	url = {https://utrecht-university.shinyapps.io/BayesSamplSizeDet-CRT/}, 
	publisher = {Utrecht University}, <br>
	author = {Barragan, Camila and Moerbeek, Mirjam}, 
	month = apr, 
	year = {2026}, 
}")),
                                br(),
                                p(strong("APA Style")),
                                p(style = "padding-left: 25px; text-indent: -25px; line-height: 1.5;",
                                  HTML("Barragan, C. & Moerbeek, M.(2026). 
                                       <i>Sample Size Determination for Bayesian Hypothesis Testing in
               Multivariate Cluster Randomised Trials </i> [Shiny app]. Posit. 
                                       https://utrecht-university.shinyapps.io/BayesSamplSizeDet-CRT/")
                                )
                            ),
                            wellPanel(
                                style = "background-color: #f0f7fb; border: none;",
                                h4(icon("comments"), "Feedback & Requests"),
                                p("For any bug, error, or addition you would like to request
                 you may contact Camila Barragán via"),
                                tags$ul(
                                    style = "list-style-type: none; padding-left: 5px;",
                                    tags$li(icon("envelope"), "Email: ", tags$a(href = 
                                                                                    "mailto:cn.barragan.ibanez@gmail.com",
                                                                                "cn.barragan.ibanez@gmail.com")),
                                    tags$li(icon("github-alt"), "GitHub: ", tags$a(href = "https://github.com/cnbi/SSD-Multivariate-CRT",
                                                                                   target = "_blank", 
                                                                                   "Repository"))
                                    
                                )
                            )
                            
                     )
                     
                 )
        )
    )
)


# Server ========================================================
server <- function(input, output) {
    
    # Select dataset
    dataset <- eventReactive(input$go, {
        req(input$go)
        dataset_tb <- switch(input$test,
                             iu = results_iu,
                             omnibus = results_omni,
                             homogeneity = results_homog)
        
        # Filter dataset based on ICCs, n1, and threshold
        filtered_data <- dataset_tb %>% filter(n1 == as.numeric(input$n1), 
                                               out_specific_ICC == as.numeric(input$rho0),
                                               intersubj_between_outICC == as.numeric(input$rho1),
                                               intrasubj_between_outICC == as.numeric(input$rho2),
                                               pmp_thresh == as.numeric(input$pmp_thres))
        
        # Filter effect sizes and delta
        if (input$test == "iu" | input$test == "omnibus") {
            filtered_data <- filtered_data %>% filter(eff_size1 == as.numeric(input$outcome1),
                                                      eff_size2 == as.numeric(input$outcome2))
        } else {
            filtered_data <- filtered_data %>% filter(
                eff_size1 == as.numeric(input$outcome1),
                delta == as.numeric(input$delta))
        }
        return(filtered_data)
    })
    
    # Give format to table and render
    hypotheses <- c("H1", "H2", "H3", "H4")
    output$tablePMP <- renderTable({
        req(input$go, dataset())
        data_set <- dataset()
        if (input$test == "iu" | input$test == "omnibus") {
            data1 <- data_set %>% 
                dplyr::select("eta.PMP1", "eta.PMP2", "eta.PMP3", "eta.PMP4") %>% 
                tidyr::pivot_longer(cols = everything(),
                                    values_to = "P(PMP.H > threshold)",
                                    names_to = NULL) %>% 
                mutate(Hypothesis = hypotheses, .before = 1)
            data2 <- data_set %>% 
                dplyr::select("mean.PMP1", "mean.PMP2", "mean.PMP3", "mean.PMP4") %>% 
                tidyr::pivot_longer(cols = everything(), values_to = "Error",
                                    names_to = NULL) %>% 
                mutate(Error = 1 - Error)
            bind_cols(data1, data2)
        } else {
            data1 <- data_set %>% 
                dplyr::select("eta.PMP1", "eta.PMP2") %>% 
                tidyr::pivot_longer(cols = everything(),
                                    values_to = "P(PMP.H > threshold)",
                                    names_to = NULL) %>% 
                mutate(Hypothesis = hypotheses[1:2], .before = 1)
            data2 <- data_set %>% 
                dplyr::select("mean.PMP1", "mean.PMP2") %>% 
                tidyr::pivot_longer(cols = everything(), values_to = "Error",
                                    names_to = NULL) %>% 
                mutate(Error = 1 - Error)
            bind_cols(data1, data2)
        }},
        
        # Make pretty table
        striped = TRUE, spacing = "l", digits = 3, width = "90%", align = "c"
    )
    
    # Make plot
    ## Select dataset and filter data
    dataset_pl <- eventReactive(input$go, {
        req(input$go)
        dataset_ <- switch(input$test,
                           iu = results_iu_plot,
                           omnibus = results_omni_plot,
                           homogeneity = results_homog_plot)
        
        # Filter dataset based on ICCs, n1, and threshold
        filtered_data <- dataset_ %>% filter(n1.final == as.numeric(input$n1), 
                                             out_specific_ICC == as.numeric(input$rho0),
                                             intersubj_between_outICC == as.numeric(input$rho1),
                                             intrasubj_between_outICC == as.numeric(input$rho2),
                                             pmp_thresh == as.numeric(input$pmp_thres))
        
        
        # Filter effect sizes and delta
        if (input$test == "iu" | input$test == "omnibus") {
            filtered <- filtered_data %>% filter(eff_size1 == as.numeric(input$outcome1),
                                                 eff_size2 == as.numeric(input$outcome2))
        } else {
            filtered <- filtered_data %>% filter(
                eff_size1 == as.numeric(input$outcome1),
                delta == as.numeric(input$delta)
            )
        }
        
        return(filtered)
    })
    
    reactive_data_plot <- reactive({
        req(dataset_pl())
        dataset_pl() %>% filter(hypothesis %in% input$hypothesis)
    })
    # Make 
    output$distPlotPMP <- renderPlot({
        req(input$go, reactive_data_plot())
        # Draw plot PMP
        ggplot(reactive_data_plot(), aes(x = PMP, color = as.factor(hypothesis), fill = as.factor(hypothesis))) + 
            geom_histogram(binwidth = 0.02, aes(y = after_stat(density)), alpha = 0.5, position = "identity") +
            geom_density(alpha = 0.2) +
            geom_vline(aes(xintercept = pmp_thresh, colour = cbbPalette[9]), linetype = "dashed") +
            scale_fill_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8], "4" = cbbPalette[4]), name = "Hypothesis") +
            scale_color_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8], "4" = cbbPalette[4]), name = "Hypothesis") +
            ylab("Density") + xlab("Posterior Model Probability") +
            theme(legend.position = "bottom", axis.title = element_text(size = 16),
                  axis.text = element_text(size = 15),
                  plot.caption = element_text(hjust = 0, size = 12),
                  plot.title = element_text(size = 14, face = "bold")) + 
            labs(title = "Posterior Model Probabilities favouring true hypothesis")
        
    })
    
    
    output$distPlotBF <- renderPlot({
        req(input$go, input$incl_bayes, reactive_data_plot())
        # Draw plot BF
        ggplot(reactive_data_plot(), aes(x = log(BF), color = as.factor(hypothesis), fill = as.factor(hypothesis))) + 
            geom_histogram(binwidth = 0.2, aes(y = after_stat(density)), alpha = 0.5, position = "identity") +
            geom_density(alpha = 0.2) +
            geom_vline(aes(xintercept = 1, colour = cbbPalette[9]), linetype = "dashed") +
            scale_fill_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8], "4" = cbbPalette[4]), name = "Hypothesis") +
            scale_color_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8], "4" = cbbPalette[4]), name = "Hypothesis") +
            ylab("Density") + xlab(bquote("log Bayes factor"["mu"])) +
            theme(legend.position = "bottom", axis.title = element_text(size = 16),
                  axis.text = element_text(size = 15),
                  plot.caption = element_text(hjust = 0, size = 12),
                  plot.title = element_text(size = 14, face = "bold")) + 
            labs(title = "Logarithm of Bayes factor favouring true hypothesis")
    })
    
    
    # Interpretation
    output$interpretation <- renderUI({
        req(input$go, dataset())
        data_ <- dataset()
        list_style <- "list-style-type: none; padding-left: 0; font-weight: bold; color: #34495e;"
        
        if (input$test == "homogeneity") {
            paragraphs <- paste0("A cluster randomised trial with", data_$n1.final,
                                 " individuals per cluster and ", data_$n2.final, 
                                 " clusters per treatment condition yields a ", round(data_$eta.PMP1 * 100, 3),
                                 "% of posterior model probabilties larger than ", as.numeric(input$pmp_thres) * 100,
                                 "% when H1 is true. Whereas when the complement hypothesis is true, ",
                                 round(data_$eta.PMP2 * 100, 3), "% of posterior model probabilties are larger than ",
                                 as.numeric(input$pmp_thres) * 100, "%."
            )
            hypo_list <- list(
                "H1: Outcome1 - Outcome2 < Delta",
                "Hc: Outcome1 - Outcome2 ≥ Delta")
        } else {
            paragraphs <- paste0("A cluster randomised trial with ", data_$n1.final,
                                 " individuals per cluster and ", data_$n2.final, 
                                 " clusters per treatment condition yields a ", round(data_$eta.PMP1 * 100, 3),
                                 "% of posterior model probabilities larger than ", as.numeric(input$pmp_thres) * 100,
                                 "% when H1 is true.  When H2 is true, ", round(data_$eta.PMP2 * 100, 3),
                                 "% of posterior model probabilities are larger than the threshold. ",
                                 "When H3 is true, ", round(data_$eta.PMP3 * 100, 3),
                                 "% of posterior model probabilities are larger than 
                       the threshold . Finally, when H4 is true, ", round(data_$eta.PMP3 * 100, 3), 
                                 "% of posterior model probabilities are larger than ",
                                 as.numeric(input$pmp_thres) * 100, "%."
                                 
            )
            hypo_list <- list(
                "H1: Outcome1 > 0 & Outcome2 > 0",
                "H2: Outcome1 > 0 & Outcome2 < 0",
                "H3: Outcome1 < 0 & Outcome2 > 0",
                "H4: Outcome1 ≤ 0 & Outcome2 ≤ 0"
            )
        }
        # Convert to HTML
        tagList(
            tags$p(paragraphs, style = "margin-bottom: 15px; line-height: 1.6;"),
            tags$ul(
                style = "list-style-type: disc; padding-left: 25px;",
                lapply(hypo_list, function(x) tags$li(x, style = "margin-bottom: 5px; font-weight: 500;"))
            )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
