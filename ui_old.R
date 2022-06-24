#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyjs)

#install shinyBS
library(shinyBS)
#install shinyWidgets
library(shinyWidgets)
library(rmarkdown)
library(magrittr)
library(ggplot2)
library(plotly)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
                  theme = shinytheme("yeti"),
                  useShinyjs(),
                  
                  # Application title
                  h1("Value of Information Analysis"),
                  h3("Version 4.1: Tabular GUI"),
                  hr(),
                 
                 # tags$hr(),
                 tags$div(
                   tags$p("User can load their own scenarios to update or create new scenarios. User can also create new scenarios with default scenario. "), 
                 ),
                 
                 fluidRow(
                   column(
                     width = 12,selectInput("ddlSelect", label="Select a Scenario:",choices = NULL)
                   )),
                 
                 fluidRow(
                   column(
                     width = 12,  fileInput("userFile", ("Load my Scenarios (optional):"),accept=".xlsx"),
                     htmlOutput("errorMessage0")
                        )),
                 
                 
                  hr(),
                 
                 
                  
                  tabsetPanel(type = "tabs",
                               tabPanel("Decision Rules and Prior Uncertainty",
                                       h3("Decision Rules"),
                                       hr(),
                                       fluidRow(
                                           column(
                                               width = 4,
                                               
                                               radioButtons(
                                                   inputId = "decisionRule",
                                                   label = HTML(paste0("<b>Decision Rule</b>")),
                                                   choices = c(
                                                     "Benefit-Risk Decision-Maker (BRDM) - Minimizing Total Social Cost" = 1,
                                                      "Target-Risk Decision-Maker (TRDM) - Reduce Exposure Above Target Risk Level" = 2
                                                   ),

                                                  selected = 1
                                               ), bsTooltip(id = "decisionRule", title = "How the decision maker implements the exposure reduction."),
                                               
                                               radioButtons(
                                                 inputId = "RS_included",
                                                 label = HTML(paste0("<b>Inclusion of Response Surface</b>")),
                                                 choices = c(
                                                   "Without response surface (seconds for TRDM, minutes for BRDM)" = 2,
                                                   "With coarse-grid response surface (minutes for TRDM, 10 minutes for BRDM)" = 3,                                                 
                                                   "With fine-grid response surface (minutes for TRDM, >40min for BRDM)" = 1
                                                 ),
                                                 selected = 1
                                               ), bsTooltip(id = "RS_included", title = "Whether to include response surface analysis."), 
                                                        
                                               numericInput(
                                                   inputId = "TRL",
                                                   label = HTML(paste0("<b>TRL</b>", " - Target Risk Level (log",tags$sub("10"),")", sep="")),
                                                   value = -1
                                               ), bsTooltip(id = "TRL", title = "Target risk level (log\U2081\U2080) used by TRDM. For fatal outcome TRL is defined over lifetime, while it is given per day for acute outcomes."),
                                               
                                               numericInput(
                                                   inputId = "ucl",
                                                   label = HTML(paste0("<b>UCL</b>", " - Upper Confidence Limit for the Uncertainty Distribution about Risk (%)", sep="")),

                                                   value = -1
                                               ), bsTooltip(id = "ucl", title = "This value will be used to compute the quantile of uncertainty distribution about risk to compare to the target risk level to determine that the chemical is safe, used by TRDM."),
 
                                               numericInput(
                                                   inputId = "lcl",
                                                   label = HTML(paste0("<b>LCL</b>", " - Lower Confidence Limit for the Uncertainty Distribution about Risk (%)", sep="")),

                                                   value = -1
                                               ), bsTooltip(id = "lcl", title = "This value will be used to compute the quantile of uncertainty distribution about risk to compare to the target risk level to determine that the chemical will be controlled, used by TRDM.")
                                           ),
                                           column(
                                               width = 8,
                                               plotOutput("priorRisk", height = 480)
                                           )
                                       ),
                                       
                                       h3("Prior Hyperparameters"),
                                       hr(),
                                       fluidRow(
                                           column(
                                               width = 4,
                                               h4("Toxicity"),
                                               numericInput(
                                                 inputId = "muMeanThreshold",
                                                label = HTML(paste0("<b>\U03BC", tags$sub("tox"),"</b>", " - Geometric Mean of the ED\U2085\U2080 (log\U2081\U2080)", sep="")),
                                                 value = -1
                                               ), bsTooltip(id = "muMeanThreshold", title = "Mean (log\U2081\U2080) of the uncertainty distribution of the mean of the threshold distribution."),
                                               
                                               numericInput(
                                                 inputId = "sigmaMeanThreshold",
                                                 label = HTML(paste0("<b>Range[u",tags$sup("0"),"(\U03BC",tags$sub("tox"),")]", "</b>", " - Range of Uncertainty about", " \U03BC", tags$sub("tox"), " (OM)", sep="")),
                                                 value = -1
                                               ), bsTooltip(id = "sigmaMeanThreshold", title = "Uncertainty in Mean Toxicity (OM). OM refers to orders of magnitude. The range is defined as 99.975% of the uncertainty distribution."),
                                               
                                               numericInput(
                                                 inputId = "muSDThreshold",
                                                 label = HTML(paste0("<b>\U03C3", tags$sub("tox"), "</b>", " - Geometric Mean of the Standard Deviation of Toxicity (log\U2081\U2080)", sep="")),
                                                 value = -1
                                               ), bsTooltip(id = "muSDThreshold", title = "Mean (log\U2081\U2080) of the uncertainty distribution of the standard deviation of the threshold distribution."),
                                               
                                               numericInput(
                                                 inputId = "sigmaSDThreshold",
                                                 label = HTML(paste0("<b>Range[u",tags$sup("0"),"(\U03C3",tags$sub("tox"),")]", "</b>", " - Range of Uncertainty about", " \U03C3", tags$sub("tox"), " (OM)", sep="")),
                                                 value = -1
                                               ), bsTooltip(id = "sigmaSDThreshold", title = "Uncertainty in the standard deviation about mean Toxicity (OM). OM refers to orders of magnitude. The range is defined as 99.975% of the uncertainty distribution."),

                                               hr(),
                                               h4("Exposure"),
                                               numericInput(
                                                   inputId = "muMeanExposure",
                                                   label = HTML(paste0("<b>\U03BC", tags$sub("exp"), "</b>", " - Geometric Mean of the Population Exposure (log\U2081\U2080)", sep="")),
                                                   value = -1
                                               ), bsTooltip(id = "muMeanExposure", title = "Mean (log\U2081\U2080) of the uncertainty distribution of the mean of the exposure distribution."),
                                               
                                               numericInput(
                                                  inputId = "sigmaMeanExposure",
                                                  label = HTML(paste0("<b>Range[u",tags$sup("0"),"(\U03BC",tags$sub("exp"),")]", "</b>", " - Range of Uncertainty about", " \U03BC", tags$sub("exp"), " (OM)", sep="")),
                                                  value = -1
                                               ), bsTooltip(id = "sigmaMeanExposure", title = "Uncertainty in Mean Exposure level (OM). OM refers to orders of magnitude. The range is defined as 99.975% of the uncertainty distribution."),
                                               
                                               numericInput(
                                                   inputId = "muSDExposure",
                                                   label = HTML(paste0("<b>\U03C3", tags$sub("exp"), "</b>", " - Geometric Mean of the Standard Deviation of Exposure (log\U2081\U2080)", sep="")),
                                                   value = -1
                                               ), bsTooltip(id = "muSDExposure (log\U2081\U2080)", title = "Uncertainty in the standard deviation about Mean Exposure (OM). OM refers to orders of magnitude. The range is defined as 99.975% of the uncertainty distribution."),
                                               
                                               numericInput(
                                                   inputId = "sigmaSDExposure",
                                                   label = HTML(paste0("<b>Range[u",tags$sup("0"),"(\U03C3",tags$sub("exp"),")]", "</b>", " - Range of Uncertainty about", " \U03C3", tags$sub("exp"), " (OM)", sep="")),
                                                   value = -1
                                               ), #bsTooltip(id = "sigmaSDExposure", title = "Standard deviation (log\U2081\U2080) of the uncertainty distribution of the standard deviation of the exposure distribution. Must be less than 1/5 of the mean."),
                                           ),
                                               
                                           column(
                                               width = 8,
                                               plotOutput("meanThresholdDistribution", height = 320)

                                           ),
                                       
                                           column(
                                             width = 8,
                                             plotOutput("meanExposureDistribution", height = 320)
                                             
                                           ),
                                       )
                              ),
                              
                              tabPanel("Toxicity Testing Information",
                                       h3("Toxicity Testing Information"),
                                       hr(),
                                       fluidPage(
                                         column(
                                           width = 4,
                                           h4("Test A"),

                                            numericInput(
                                             inputId = "a_sizeThreshold",
                                             label = HTML(paste0("<b>Range[u",tags$sup("A"),"(\U03BC",tags$sub("tox"),")]", "</b>", " - Range of Residual Uncertainty about", " \U03BC", tags$sub("tox"), " (OM)", sep="")),
                                             value = 0
                                           ), bsTooltip(id = "a_sizeThreshold", title = "Residual Uncertainty in Mean Toxicity (OM). OM refers to orders of magnitude. (eg, 3 OM means that the 99.975% CI spans 1,000-fold range)"),
                                           
                                           numericInput(
                                             inputId = "a_delay",
                                             label = HTML(paste0("<b>Delay</b> (Years)", sep="")),
                                             value = -1
                                           ), bsTooltip(id = "a_delay", title = "Delay in decision-making due to collection and analysis of Test A."),
                                           
                                           numericInput(
                                             inputId = "a_costPerTest",
                                             label = HTML(paste0("<b>Cost Per Test</b> (USD)", sep="")),
                                             value = -1
                                           ), bsTooltip(id = "a_costPerTest", title = "Cost of acquisition of information for Test A."),
                                           
                                           br(),
                                           
                                           h4("Test B"),
                                           numericInput(
                                             inputId = "b_sizeThreshold",
                                             label = HTML(paste0("<b>Range[u",tags$sup("B"),"(\U03BC",tags$sub("tox"),")]", "</b>", " - Range of Residual Uncertainty about", " \U03BC", tags$sub("tox"), " (OM)", sep="")),
                                             value = -1
                                           ), bsTooltip(id = "b_sizeThreshold", title = "Residual Uncertainty in Mean Toxicity (OM). OM refers to orders of magnitude. (eg, 3 OM means that the 99.975% CI spans 1,000-fold range)"),
                                           
                                           numericInput(
                                             inputId = "b_delay",
                                             label = HTML(paste0("<b>Delay</b> (Years)", sep="")),
                                             value = -1
                                           ), bsTooltip(id = "b_delay", title = "Delay in decision-making due to collection and analysis of Test B."),
                                           
                                           numericInput(
                                             inputId = "b_costPerTest",
                                             label = HTML(paste0("<b>Cost Per Test</b> (USD)", sep="")),
                                             value = -1
                                           ), bsTooltip(id = "b_costPerTest", title = "Cost of acquisition of information for Test B.")
                                         )
                                       )
                              ),
                              
                              tabPanel("Economic Parameters",
                                       h3("Economic Parameters"),
                                       hr(),
                                       fluidRow(  
                                           column(
                                               width = 4,

                                               radioButtons(
                                                   inputId = "endpoint",
                                                   label = HTML(paste0("<b>Fatal or Acute Health Outcomes</b>")),
                                                   choices = c(
                                                       "Fatal" = 1,
                                                       "Acute" = 2
                                                   ),
                                                   selected = 1
                                               ), bsTooltip(id = "endpoint", title = "Whether the predicted response is a fatal or acute health outcome."),
                                               
                                               numericInput(
                                                 inputId = "vsl",
                                                 label = HTML(paste0("<b>VSL</b>", " - Value of a Statistical Life (Millions of USD) - [For Fatal Outcome]", sep="")),
                                                 value = -1
                                               ), bsTooltip(id = "vsl", title = "Value of Statistical Life (VSL) (Millions of USD)."),

                                              numericInput(
                                                   inputId = "costPerAcuteCase",
                                                   label = HTML(paste0("<b>Cost Per Acute Case</b>"," (USD) - [For Acute Outcome]", sep="")),
                                                   value = -1
                                               ), bsTooltip(id = "costPerAcuteCase", title = "Average cost per acute case. (USD)"),
                                               
                                               numericInput(
                                                   inputId = "By",
                                                   label = HTML(paste0("<b>B<sub>y</sub></b>"," - Annualization Factor", sep="")),
                                                   value = -1
                                               ), bsTooltip(id = "By", title = "Annualize risk/health cost. If Fatal outcome is used, lifetime risk is converted to annual risk (by dividing by life expencancy), while acute outcomes will be converted from daily risk."),
                                              
                                               numericInput(
                                                   inputId = "population",
                                                   label = HTML(paste0("<b>Population</b>")),
                                                   value = -1
                                               ), bsTooltip(id = "population", title = "Size of exposed population (persons)."),
                                               
                                               numericInput(
                                                   inputId = "sdr",
                                                   label = HTML(paste0("<b>Discount Rate</b>", " (%)", sep="")),
                                                   value = 0,
                                                   min=0,
                                                   max=100,
                                               ), bsTooltip(id = "sdr", title = "Discount Rate (%). Example: for 5%, enter 5."),
                                               
                                               numericInput(
                                                   inputId = "t0",
                                                   label = HTML(paste0("<b>Year 1</b>")),
                                                   value = -1
                                               ), bsTooltip(id = "t0", title = "Starting year (for NPV calculation)."),
                                               
                                               numericInput(
                                                   inputId = "tHorizon",
                                                   label = HTML(paste0("<b>Final Year</b>")),
                                                   value = -1
                                               ), bsTooltip(id = "tHorizon", title = "Final year (for NPV calculation)."),
                                               
                                               numericInput(
                                                   inputId = "annualReductionCost",
                                                   label = HTML(paste0("<b>ACC<sub>max</sub></b>", " - Maximum Annual Control Cost (USD per year)", sep="")),
                                                   value = -1
                                               ), bsTooltip(id = "annualReductionCost", title = "Annual cost for 100% exposure reduction (USD per year)."),
                                               
                                               numericInput(
                                                   inputId = "controlCostExponent",
                                                   label = HTML(paste0("<b>\U03B7</b>", " - Control Cost Exponent", sep="")),
                                                   value = 0,
                                                   min=0,
                                                   max=20,
                                               ),  bsTooltip(id = "controlCostExponent", title = "This parameter controls the degree of nonlinearity of the control cost curve to the right. A value of 0 creates a linear cost function. Values greater than 0 controls the steepness of the curve at higher levels of exposure reduction.")
                                           ),
                                           # Graphical Feedback
                                           column(
                                             width = 8,
                                             plotOutput("sdr", height = 480)
                                           ),
                                           
                                           # Graphical Feedback
                                           column(
                                             width = 8,
                                             plotOutput("controlCostExponent", height = 480)
                                           )
                                       )
                              )
                              
                  ),
                  
                tags$hr(),
                
                
                fluidRow(
                  column(width = 4),
                  column(width = 3, 
                        textInput(
                          inputId = "ScenarioName",
                          width = "170px",
                          label = "Please enter a name for new scenario:",
                          value = ""                              
                        ),  bsTooltip(id = "ScenarioName", title = "new scenario name."),
                   
                        bsButton(
                          inputId = "Create",
                          label = "Create a New Scenario",
                          style = "success",
                          size = "small",
                          width = "170px",
                          icon = icon("save"),
                          block = FALSE
                        ), bsTooltip(id = "Create", title = "Click to create a new scenario.", placement = "top"),
                        htmlOutput("errorMessage")),
                  
                  
                  column(width = 3,
                         textInput(
                           inputId = "ScenarioNameUpdate",
                           width = "170px",
                           label = "Please enter a new scenario name to update:",
                           value = ""
                         ),  bsTooltip(id = "ScenarioNameUpdate", title = "Scenario's new name."),
                         
                         bsButton(
                           inputId = "Update",
                           width = "170px",
                           label = "Update Values",
                           style = "success",
                           size = "small",
                           icon = icon("edit"),
                           block = FALSE
                         ), bsTooltip(id = "Update", title = "Click to update current values.", placement = "top"),
                         htmlOutput("errorMessage2")
                        ),
               
                  column(width = 2, 
                         tags$br(),
                         tags$br(),
                         downloadButton("downloadData", "Get a Copy of my Scenarios", style="background-color:#5cb85c;color:white; font-size:small"),
                         tags$br(),
                         tags$br(),
                         downloadButton("downloadComputedResult", "Compute and Get Results", style="background-color:#5cb85c;color:white; font-size:small")
                  )),
               
                tags$hr()
                
                
)
)