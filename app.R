################################################################################
#  Sample Size Determination for a Multivariate Cluster Randomised Trial w/ BF #
################################################################################

# Libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Data
results_FindN2_IU <- readRDS("data/results_FindN2_IU.RDS")
results_FindN2_omni <- readRDS("data/results_FindN2_omni.RDS")
results_FindN2_homoge <- readRDS("data/results_FindN2_homoge.RDS")

# Palette for plots
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#661100")

# Define UI 
ui <- fluidPage(
    theme = shinythemes::shinytheme("paper"),
    
    # Application title and window
    titlePanel("Sample Size Determination for a Multivariate Cluster Randomised Trials with Bayes Factor",
               windowTitle = "Bayes Sample Size Determination: Multivariate CRT"),
    
    # Panel in the left side
    tabPanel("Results",
             sidebarLayout(
                 sidebarPanel(
                     # Widgets for inputs
                     radioButtons("test",
                                  "The test that will be used",
                                  choices = c("Intersection-union" = "iu",
                                              "Homogeneity" = "homog",
                                              "Omnibus" = "omni")),
                     conditionalPanel(
                         condition = "input_test == 'homog'",
                         selectInput("delta",
                                     "Clinically important difference",
                                     choices = c("0.2" = 0.2,
                                                 "0.3" = 0.3))),
                     selectInput("n1",
                                 "Cluster sizes:",
                                 choices = c("5" = 5,
                                             "15" = 15,
                                             "30" = 30)),
                     selectInput("rho0",
                                 "Outcome-specic intraclass correlation",
                                 choices = c("0.01" = 0.01,
                                             "0.05" = 0.05)),
                     selectInput("rho1",
                                 "Intersubject between-outcome intraclass correlation",
                                 choices = c("0.01" = 0.005,
                                             "0.05" = 0.025)),
                     selectInput("rho2",
                                 "Intrasubject  between-outcome intraclass correlation",
                                 choices = c("0.01" = 0.2,
                                             "0.05" = 0.5)),
                     #TODO: Conditional the effect 2 on effect 1
                     conditionalPanel(
                         condition = "input_test == 'iu'",
                         selectInput("treat_eff1",
                                     "Treatment effect 1",
                                     choices = c("0.3" = 0.3,
                                                 "0.5" = 0.5,
                                                 "0.7" = 0.7,
                                                 "0.9" = 0.9))),
                     conditionalPanel(
                         condition = "input_test == 'iu' && input.treat_eff1 == 0.3",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.5" = 0.5,
                                                 "0.7" = 0.7,
                                                 "0.9" = 0.9))
                     ),
                     conditionalPanel(
                         condition = "input_test == 'iu' && input.treat_eff1 == 0.5",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.7" = 0.7,
                                                 "0.9" = 0.9))
                     ),
                     conditionalPanel(
                         condition = "input_test == 'iu' && input.treat_eff1 == 0.7",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.9" = 0.9))
                     ),
                     conditionalPanel(
                         condition = "input.test == 'homog'",
                         selectInput("treat_eff1",
                                     "Treatment effect",
                                     choices = c("0.2" = 0.2,
                                                 "0.5" = 0.5,
                                                 "0.8" = 0.8))),
                     conditionalPanel(
                         condition = "input_test == 'homog' && input.treat_eff1 == 0.2",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.3" = 0.3))
                     ),
                     conditionalPanel(
                         condition = "input.test == 'homog' && input.treat_eff1 == 0.5",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.6" = 0.6))
                     ),
                     conditionalPanel(
                         condition = "input_test == 'homog' && input.treat_eff1 == 0.8",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.9" = 0.9))
                     ),
                     conditionalPanel(
                         condition = "input_test == 'omni' && input.treat_eff1 == 0.2",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.3" = 0.3,
                                                 "0.5" = 0.5,
                                                 "0.7" = 0.7,
                                                 "0.9" = 0.9))
                     ),
                     conditionalPanel(
                         condition = "input_test == 'omni' && input.treat_eff1 == 0.3",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.5" = 0.5,
                                                 "0.7" = 0.7,
                                                 "0.9" = 0.9))
                     ),
                     conditionalPanel(
                         condition = "input_test == 'omni' && input.treat_eff1 == 0.5",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.7" = 0.7,
                                                 "0.9" = 0.9))
                     ),
                     conditionalPanel(
                         condition = "input_test == 'omni' && input.treat_eff1 == 0.7",
                         selectInput("treat_eff2",
                                     "Treatment effect 2",
                                     choices = c("0.9" = 0.9))
                     ),
                 )
             )
             
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("distPlot")
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
