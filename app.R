library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Jack Tseng workout2: Financial modalities"),
  fluidRow(
    column(4, 
           
           # slider inputs 
           sliderInput("init",
                       "Initial Amount:",
                       min = 0,
                       max = 100000,
                       value = 1000, step = 500),
           sliderInput("ann_contrib",
                       "Annual Contribution:",
                       min = 0,
                       max = 50000,
                       value = 2000, step = 500)),
    column(4,
           sliderInput("ret_rate",
                       "Return Rate: (in %)",
                       min = 0,
                       max = 20,
                       value = 5, step = 0.1),
           sliderInput("growth_rate",
                       "Growth Rate: (in %)",
                       min = 0,
                       max = 20,
                       value = 2, step = 0.1)),
    
    column(4,
           sliderInput("years",
                       "Years:",
                       min = 0,
                       max = 50,
                       value = 20, step = 1),
           selectizeInput("facet",
                          "Facet?",
                          choices = c("Yes", "No")))
  ),
  
  h4("Timelines"),
  # Show the Timelines Plot
  plotOutput("TimelinesPlot"),
  h4("Balances"),
  tableOutput("dataframe")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({
    future_value <- function(amount, rate, years) {
      x = amount*(1+rate)^years
      return(x)
    }
    annuity <- function(contrib, rate, years) {
      x = contrib*(((1+rate)^years-1)/rate)
      return(x)
    }
    growing_annuity <- function(contrib, rate, growth, years) {
      x = contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
      return(x)
    }
    no_contrib <- c()
    fixed_contrib <- c()
    growing_contrib <- c()

    
    
    for (i in 1:input$years){
      no_contrib[i] = future_value(input$init,input$ret_rate / 100, i-1)
      
    }
    for (i in 1:input$years){
      fixed_contrib[i] = future_value(input$init,input$ret_rate / 100, i-1) + 
        annuity(input$ann_contrib, input$ret_rate / 100, i-1)
    }
    for (i in 1:input$years){
      growing_contrib[i] = future_value(input$init,input$ret_rate / 100, i-1) + 
        growing_annuity(input$ann_contrib, input$ret_rate / 100, input$growth_rate / 100, i-1)
    }
    year = c(1:input$years)
    modalities <- data.frame("year" = year, "no_contrib" = no_contrib ,
                             "fixed_contrib" = fixed_contrib, 
                             "growing_contrib" = growing_contrib)
  })
  
  output$TimelinesPlot <- renderPlot({
    # generate plot based on inputs from ui.R
    
    
    library(ggplot2)
    if (input$facet == "Yes"){
      library(reshape2)
      d <- melt(data(), id = 1:3, measure = c("no_contrib","fixed_contrib","growing_contrib"))
      library(ggplot2)
      graph_modes <- ggplot(
        data = d, aes(x = year, y = value, color = variable)) + 
        ggtitle("Three different investment techniques") +
        geom_area(mapping = aes(fill = variable)) +
        labs(x = "Time", y = "Growth") + 
        facet_wrap(~variable)
    } else{
      graph_modes <- ggplot(data = data(), aes(x = year)) + 
        ggtitle("Three different investment techniques") +
        geom_line(aes(y = no_contrib, color = "no_contrib")) +
        geom_line(aes(y = fixed_contrib, color = "fixed_contrib")) +
        geom_line(aes(y = growing_contrib, color = "growing_contrib")) +
        labs(x = "Time", y = "Growth") 
    }
    graph_modes
  })
  
  
  
  output$dataframe <- renderTable({
    # generate plot based on inputs from ui.R
    data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)