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
              fileInput("samplefile", "Choose CSV File",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"),
                        placeholder = "example_intensity_data_subset_69.csv"),
              
              sliderInput("bins",
                         "Number of bins:",
                         min = 1,
                         max = 50,
                         value = 30)
          ),
          
          mainPanel(
            tabsetPanel(
                tabPanel("Summary", # Show a summary of the sample distribution
                    p("Placeholder")
                ),
                
                tabPanel("Table",
                    p("Placeholder")
                ),
                
                tabPanel("Plots",
                )
            )
          )
        )
      ),
     
     # Counts tab
     tabPanel("Counts",
       sidebarLayout(
           # Sidebar
           
           sidebarPanel(
               # Input: Select a file ----
               fileInput("file", "Upload *NORMALIZED* Counts Matrix File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"),
                         placeholder = c("example_intensity_data_subset_69.csv")
                         ),
               
               # HORIZONAL LINE
               hr(),
               
               # Heading
               h4("Filter Counts Data According to Statistical Params:"),
               
               # Input: Percent Variance Slider
               sliderInput("var",
                           "% Variance",
                           min = 0,
                           max = 100,
                           value = 80
               ),
               
               # Input: Min Number of Non-zero Samples Slider
               sliderInput("zero",
                           "Minimun Number of Non-Zero Samples:",
                           min = 0,
                           max = 35,
                           value = 5)
           ),
           
           mainPanel(
              # Counts table panel
              tabsetPanel(
                  tabPanel("Summary",
                      # Summary table
                      p("Summary of Counts File"),
                      
                      verbatimTextOutput("summaryTable")
                  ),
                  
                  tabPanel(
                      "Head",
                      p("Counts File Table"),
                      tableOutput(outputId = "countsTable")
                  ),
                  
                  tabPanel("Plots",
                      p("Scatterplot of My Counts")
                      # consider putting a colorpicker here
                  ),
                  
                  tabPanel("Heatmap",
                      p("Clustered Heatmap of filtered Counts")
                  ),
                  
                  tabPanel("PCA",
                      p("Principal Component Analysis of Filtered Genes")
                  )
              )
              # counts table
              #shit goes here
           )
       ),
      ),
     
     # DEs Tab
     tabPanel("DE",
        # Sidebar
        sidebarLayout(
            sidebarPanel(
                # more stuff
                h2("Differential Expression"),
                
                # Input: Select a DE analysis type~~~~~~~
                radioButtons(
                    inputId = "de",
                    label = "Differential Expression Analysis:",
                    choices = c("DESeq",
                                "edgeR",
                                "limma"),
                    selected = "DESeq"
                )
            ),
            
            # DEs plot panel
            mainPanel(
                # Diff Eq Result DataTable
                tabPanel("Table",
                         DT::dataTableOutput("deTable")
                ),
                
                # Differential Expression plot
                tabPanel("DE Plot")
            )
        ),
        
      ),
    )
)






# Define server logic required to draw a histogram
server <- function(input, output) {
    #' 
    #' loads data and returns dataframe
    load_data <- reactive({
        return(read.csv(input$file$datapath))
    })
    
    load_sample <- reactive({
        req(input$samplefile)
        return(read.csv(input$samplefile$datapath))
    })
    
    ## Server functions go here
    
    #' 
    #' Summarize the data from input file
    #' For column in data, display the name,
    #' Datatype, and Mean or discrete
    #' values depending if data are 
    #' numerical or categorical.
    #' 
    data_summary <- function(input_data) {
        summary.data.frame(input_data)
    }
    
    
    ## Output Elements go here
    
    #' generate a table and display its head (first 6 rows)
    output$countsTable <- renderTable({
        # table
        req(input$file)
        head(load_data())
    })
    
    output$summaryTable <- renderTable({
        # table
        req(input$file)
        
        summary_table <- data_summary(
            load_data())
        
        return(summary_table)
    })
    
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # diff_expr <- load_sample()
    output$deTable <- DT::renderDataTable({
        req(input$samplefile)
        DT::datatable(load_sample(), options = list(orderClasses = TRUE))#, pageLength = 10)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

