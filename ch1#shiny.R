library(shiny)
library(ggplot2)
library(plotly,warn.conflicts = FALSE)
library(shinythemes)

# Define UI for application 
ui <- fluidPage(theme = shinytheme("superhero"),
                tabsetPanel(
                  tabPanel( title = "Plot",
                            # Sidebar layout with a input and output definitions
                            sidebarLayout(
                              # Inputs
                              sidebarPanel(numericInput("size", "Point size", 1, 1),
                                           checkboxInput("fit", "Add line of best fit", FALSE),
                                selectInput(inputId = "data", 
                                            label = "Topic:",
                                            choices = c("Total GDP"        = "df_gdp", 
                                                        "Percapita"       = "df_percapita",
                                                        "Manufacturing"="df_mfg",
                                                        "Investment"="df_investment",
                                                        "Consumption"="df_consumption"), 
                                            selected = "Total GDP"),
                                # Select variable for y-axis
                                selectInput(inputId = "y", 
                                            label = "Value",
                                            choices = c("Value"        = "value" 
                                                        ), 
                                            selected = "value"),
                                
                                # Select variable for x-axis
                                selectInput(inputId = "x", 
                                            label = "Year:",
                                            choices = c("Year"= "Year"), 
                                            selected = "Year"),
                                
                                # Select variable for color
                                selectInput(inputId = "z", 
                                            label = "Color by:",
                                            choices = c("Country" = "variable"),
                                            selected = "Country"),
                                sliderInput(inputId = "alpha",label = "Years:",min = 1967,max = 2018,
                                            value = c(1950, 2018))  
                              ),
                              
                              # Outputs
                              mainPanel(
                                plotlyOutput(outputId = "scatterplot")
                              )
                            )
                            ),
                  
                  tabPanel(title = "Table",tableOutput("table"))
                )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  base <- reactive ({get(input$data) })
  output$scatterplot <- renderPlotly({
    ggplotly({
      mydata = base()
      mydata1 <- subset(mydata,Year >= input$alpha[1] & Year <= input$alpha[2])
      p<- ggplot(data = mydata1, aes_string(x=input$x, y = input$y,
                                       color = input$z)) +
        geom_line(size = input$size) + xlab("Year") + ylab(input$y)
      if (input$fit) {
        p <- p + geom_smooth(method = "lm")
      }
      p
      
    })
  })
  output$table <- renderTable({
    data <- base()
    data
  })
}

shinyApp(ui = ui, server = server)