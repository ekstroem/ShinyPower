library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Power calculation for two-sided comparison of means"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(

      checkboxInput("plotnulldist", "Plot Null distribution density", TRUE),
      checkboxInput("plotaltdist", "Plot alternative distribution density", TRUE),

      checkboxInput("typeIerr", "Mark type I error", FALSE),

      sliderInput("n", "n (in each group):",
                  min = 10, max = 200, value = c(50), step= 1),

      sliderInput("spread", "sd (in each group):",
                  min = 1, max = 20, value = c(3), step= 1),

      sliderInput("delta", "SMD:",
                  min = 0, max = 5, value = c(1), step= 0.1),

      sliderInput("alpha", "Significance level:",
                  min = 0.01, max = 0.1, value = c(0.05), step= 0.01)

  ),

  # Show the caption and plot of the requested variable against mpg
    mainPanel(
        h3(textOutput("caption")),
        plotOutput("dataPlot"),
        plotOutput("mpgPlot")
        )
    ))
