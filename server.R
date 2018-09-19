#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source("./dev.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  p0 <- reactive({as.numeric(input$p0)})
  N <- reactive({as.numeric(input$N)})
  gens <- reactive({as.numeric(input$gens)})
  runs <- reactive({as.numeric(input$runs)})
  

  rerun.sim <- eventReactive(input$go,ignoreNULL = F,{
    validate(
      #need((input$nPop==length(p())|length(p())==1),"number of populations must equal number of starting allele frequencies."),
      need(input$N<1000001,"Please select < 1000001 individuals")
    )
    sim.output <- simple_drift(p0=input$p0,N=input$N,gens=input$gens,runs=input$runs)
    sim.output
  })
  
  plot.data <- eventReactive(rerun.sim(),{
    meltdata(results.df=rerun.sim(),gens=input$gens,runs=input$runs)
  })
  
  output$plot <- renderPlot({
    #sim.output <- simple_drift(p0=input$p0,N=input$N,gens=input$gens,runs=input$runs)
    #sim.output.melt <- meltdata(results.df=sim.output,gens=input$gens,runs=input$runs)
    plotSim(df=plot.data(),runs=input$runs,gen=input$gens)
  })
  
  output$table <- renderTable({
    p <- rerun.sim()[input$gens+1,1:input$runs]
    nFixed <- length(p[p==1])
    nLost <- length(p[p==0])
    
    ft.ht <- getibd(input$N,input$gens)
    
    p.stats <- rerun.sim()[(input$gens+1),c("meanp","varp")]
    final.table <- rbind(c(p.stats,ft.ht[1],ft.ht[2],nFixed,nLost))
    colnames(final.table) <- c("Mean p","Var(p)","Prob. IBD","Het","Fixed","Lost")
    final.table
  })
  
})
