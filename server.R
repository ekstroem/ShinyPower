library(shiny)
library(datasets)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))


makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  # Compute the forumla text in a reactive expression since it is
  # shared by the output$caption and output$mpgPlot expressions
#  formulaText <- reactive({
#    paste("mpg ~", input$variable)
#  })

  # Return the formula text for printing as a caption
#  output$caption <- renderText({
#    formulaText()
                                        #  })

                output$dataPlot <- renderPlot({

                    par(mar=c(4,4,0,0)+.1, cex=1.4)

                    n <- input$n
                    delta <- input$delta
                    alpha <- input$alpha
                    spread <- input$spread/sqrt(n)

                    x  <- seq(-3.5, 9, length = 400)

                    plot(x, dnorm(x, mean=0, sd=spread), type="l", lwd=2,
                         xlim=range(x), ylim=range(dnorm(x, mean=0, sd=spread)),
                         xlab="Densities for mean estimates in the two populations", ylab="")
                    lines(x, dnorm(x, mean=delta, sd=spread), lwd=2)

                    text(0, dnorm(0, mean=0, sd=spread)/2, paste("Sample 1\nmean\ndistribution"))
                    text(delta, dnorm(delta, mean=delta, sd=spread)/4, paste("Sample 2\nmean\ndistribution"))

                    segments(0, 0, delta, 0, xpd = FALSE, lwd=6, col=makeTransparent("blue", alpha=80))

                })

  # Generate a plot of the requested variable against mpg and only
  # include outliers if requested
  output$mpgPlot <- renderPlot({

    par(mar=c(4,4,0,0)+.1, cex=1.4)

      n <- input$n
      delta <- input$delta
      alpha <- input$alpha
      spread <- input$spread

      x  <- seq(-4, 9, length = 200)

      # Draw the null distribution

      if (input$plotnulldist==TRUE) {
        plot(x, dnorm(x, mean=0, sd=1), type="l", xlim=range(x), ylim=range(dnorm(x, mean=0, sd=1)),
             xlab="True difference in means between groups", ylab="")
      } else {
        plot(0, -1, xlim=range(x), ylim=range(dnorm(x, mean=0, sd=1)),
             xlab="True difference in means between groups", ylab="")
      }

      ul <- qnorm(1-alpha/2, mean=0, sd=1)
      ll <- qnorm(alpha/2, mean=0, sd=1)

      segments(ll, -.02, ul, -.02, xpd = TRUE, lwd=6, col=makeTransparent("darkgreen", alpha=90))
      polygon(c(ll, ll, ul, ul), c(0, .5, .5, 0), col=makeTransparent("darkgreen", alpha=50), border=NA)


      # Draw the range that is not rejected

                                        # Draw the alternative distribution

      ncp <- sqrt(n/2)*delta/spread

      if (input$plotaltdist==TRUE) {
        lines(x, dnorm(x, mean=ncp, sd=1), lwd=3)

        # Colour the area under the alternative distribution
        x.u <- seq(ul, max(x), length=200)
        x.l <- seq(min(x), ll, length=200)

        redcol <- makeTransparent("red", alpha=50)

        polygon(c(ul, x.u, max(x.u)), c(0, dnorm(x.u, mean=ncp, sd=1), 0), col=redcol)
        polygon(c(min(x.l), x.l, ll), c(0, dnorm(x.l, mean=ncp, sd=1), 0), col=redcol)

        segments(0, -.03, ncp, -.03, xpd = TRUE, lwd=6, col=makeTransparent("darkblue", alpha=80))


      }

  if (input$typeIerr==TRUE) {
    tmpx <- c(min(x), seq(min(x), ll, length=100), ll)
    polygon(c(min(x), tmpx, ll), c(0, dnorm(tmpx, mean=0, sd=1), 0), density=45)
  }




      # Add text
     text(7, .3, paste("Power: ", round(pnorm(ll, mean=ncp, sd=1) + 1-pnorm(ul, mean=ncp, sd=1), 3)))
    #boxplot(as.formula(formulaText()),
    #        data = mpgData,
    #        outline = input$outliers)
  })

})
