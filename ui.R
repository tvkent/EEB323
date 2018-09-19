library(shiny)
source("dev.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage( #create the overall page
  
  # Application title
  titlePanel("EEB323"),
  
  # Some helpful information
  helpText("This app simulates a population under the influence of drift alone."),
  
  # Sidebar with a radio box to input which trait will be plotted
  sidebarLayout(
    sidebarPanel(
      actionButton("go","Re-run Simulation",width="100%"),
      br(),br(),
      sliderInput("p0","Starting allele frequency:",
                  min=0,max=1,
                  value=0.5,step=0.01),
      numericInput("N","Population size (diploid individuals):",
                  min=2,max=1000000,
                  value=100,step=10),
      sliderInput("gens","Number of generations to simulate:",
                  min=10, max=1000,
                  value=50, step=10),
      sliderInput("runs","Number of simulations to run:",
                  min=1,max=100,
                  value=10,step=1)
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
))
