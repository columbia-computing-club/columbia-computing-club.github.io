# make the plots for your app

library(shiny)
library(ggplot2)
hospital = read.csv("/Users/juliawrobel/Documents/Presentations and Posters/CBCC/How to use Shiny/Hospital.csv")

hospital$MEDSCHL = factor(hospital$MEDSCHL, levels = 1:2, 
                          labels = c("yes", "no"))
hospital$REGION = factor(hospital$REGION, levels = 1:4, 
                         labels = c("NE", "NC", "S", "W"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$continuousPlot <- renderPlot({
    
    # get user selected continuous variable
    contVar <- hospital[, input$contVar] 
    
    # draw the histogram with the specified number of bins
    ggplot(hospital, aes(x = contVar)) + theme_bw() + geom_histogram(fill = "pink")
    
  })
  
})
