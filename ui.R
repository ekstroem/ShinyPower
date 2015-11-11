library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Power calculation for two-sided comparison of means"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
#    selectInput("variable", "Variable:",
#                list("Cylinders" = "cyl", 
#                     "Transmission" = "am", 
#                     "Gears" = "gear")),

    checkboxInput("plotnulldist", "Plot Null distribution density", TRUE),
    checkboxInput("plotaltdist", "Plot alternative distribution density", TRUE),
    

    checkboxInput("typeIerr", "Mark type I error", FALSE),
    
    
    # Decimal interval with step value
    sliderInput("alpha", "Significance level:", 
                min = 0.01, max = 0.1, value = c(0.05), step= 0.01),
    
    
    sliderInput("delta", "SMD:", 
                min = 0, max = 5, value = c(0), step= 0.1)
    
      
  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),

    plotOutput("mpgPlot")
  )
))
