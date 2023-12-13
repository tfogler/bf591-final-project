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
   
   # UI Heading
   tags$head(
        # Application title
        titlePanel("TFogler RShiny Final Project"),
        h1("Final Project RShiny App")
   ),
   
   
   
   #$ Body Starts Here $
   # Tabs set with 'Samples', 'Counts', and 'DEs' pages
   tabsetPanel(
     # Sample tab
     tabPanel("Sample",
       
       # Sidebar with a slider input for number of bins 
       sidebarLayout(
          sidebarPanel(
              # Input: Select a file ----
              fileInput("file", "Choose CSV File",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"),
                        placeholder = "counts_data.csv"),
              
              sliderInput("bins",
                         "Number of bins:",
                         min = 1,
                         max = 50,
                         value = 30)
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
             # plotOutput("distPlot")
             tableOutput("countsTable")
          )
        )
      ),
     
     # Counts tab
     tabPanel("Counts",
       # Sidebar
       sidebarLayout(
           sidebarPanel(
               # stuff here
           ),
           
           # Counts table panel
           mainPanel(
               # counts table
           )
       ),
      ),
     
     # DEs Tab
     tabPanel("DE",
        # Sidebar
        sidebarLayout(
            sidebarPanel(
                # more stuff
            ),
            
            # DEs plot panel
            mainPanel()
        ),
        
      ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    load_data <- reactive({
        return(read.csv(input$file$datapath))
    })
    
    output$countsTable <- renderTable({
        # table
        head(load_data())
    })
    
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

