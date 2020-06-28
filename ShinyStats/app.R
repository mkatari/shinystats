#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Number of samples"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of samples to take:",
                        min = 1,
                        max = 10,
                        value = 5),
            sliderInput("sim",
                        "Number of simulations",
                        min = 100,
                        max = 10000,
                        value = 1000,
                        step = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        Placebo = c(54,51,58,44,55,52,42,47,58,46)
        Drug = c(54,73,53,70,73,68,52,65,65,60)
        
        D_placebo=numeric()
        D_drug= numeric()
        set.seed(123)
        for (i in 1:input$sim) {
            D_placebo[i] = mean(Placebo[sample(1:length(Placebo), 
                                               input$n, replace=T)])
            D_drug[i] = mean(Drug[sample(1:length(Drug), 
                                         input$n, 
                                         replace=T)])
        }
        datamelt = melt(cbind(D_placebo,D_drug), 
                        varnames = c("id","treatment"))
        
        ggplot(datamelt) + geom_histogram(mapping=aes(x=value, 
                                                      fill=treatment),
                                          bins=50)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
