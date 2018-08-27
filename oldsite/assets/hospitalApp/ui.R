# ui.R

library(shiny)
# variable designations
cats = c("MEDSCHL", "REGION")
conts = c("LOS", "AGE", "INFRISK", "CULT", "XRAY", 
          "BEDS", "CENSUS", "NURSE", "FACS")


# Define UI for application that draws a histogram for a selected continuous variable and bar plot for categorical variable
shinyUI(fluidPage(
  tabsetPanel(
    tabPanel(title = "tab 1",
             # Application title
             titlePanel("Hospital Data"),
             
             # Sidebar with a select input to select a variable from the dataset
             sidebarLayout(
               sidebarPanel(
                 selectInput("contVar", label = h4("Select a continuous variable"), 
                             choices = as.list(conts), 
                             selected = 1)
               ),
               
               # Show a histogra and barplot of a continuous and categorical varaible
               mainPanel(
                 h3("Histogram of a continous variable"),        
                 plotOutput("continuousPlot")
               )
             ) # end of tab 1
             
    ),
    tabPanel(title = "tab 2")
  )
  
))
