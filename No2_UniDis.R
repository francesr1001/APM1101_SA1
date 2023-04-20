library(shiny)
#INSTALL SHINY FIRST :)

ui_univardata <- fluidPage(
  
  titlePanel("Univariate Discrete Random Variable LA GENERATOR"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("n", label = "No. of values:", value = 1, min = 1),
      textInput("values", label = "Values:", placeholder = "Separate values by commas"),
      textInput("probs", label = "Probabilities:", placeholder = "Separate probabilities by commas")
    ),
    
    mainPanel(
      h3("OUTPUT DATA:"),
      verbatimTextOutput("summary"),
      plotOutput("pdf_plot"),
      plotOutput("cdf_plot")
    )
  )
) 


server_univardata <- function(input, output) {
  
  output$summary <- renderText({
    values <- as.numeric(unlist(strsplit(input$values, ",")))
    probs <- as.numeric(unlist(strsplit(input$probs, ",")))
    
    if (any(probs < 0) || any(probs > 1) || abs(sum(probs) - 1) > 1e-5) {
      "INVALID- interval [0,1] and sum to one."
    } else {
      new_mean <- sum(values * probs)
      new_var <- sum((values - new_mean)^2 * probs)
      
      paste0("Mean: ", round(new_mean, 2), "\nVariance: ", round(new_var, 2))
      
      x <- seq(min(values), max(values), length.out = 80)
      y_pdf <- dpois(x, lambda = new_mean)
      y_cdf <- ppois(x, lambda = new_mean)
      
      output$pdf_plot <- renderPlot({
        plot(x, y_pdf, type = "h", lwd = 2, main = "PDF")
      })
      
      output$cdf_plot <- renderPlot({
        plot(x, y_cdf, type = "s", lwd = 2, main = "CDF")
      })
    }
  })
}


shinyApp(ui = ui_univardata, server = server_univardata)
