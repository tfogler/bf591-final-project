#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(colourpicker)

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
              
              # sliderInput("bins",
              #            "Number of bins:",
              #            min = 1,
              #            max = 50,
              #            value = 30)
          ),
          
          mainPanel(
            tabsetPanel(
                tabPanel("Summary", # Show a summary of the sample distribution
                    p("Placeholder"),
                    
                    # Summary Table
                    tableOutput(outputId = "summaryTable")
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
                      p("Summary of Counts File")
                      
                      # verbatimTextOutput("summaryTable")
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
                ),
                
                # Input: Column Select buttons
                radioButtons(inputId = "xcol",
                             label = "Choose the column for the x-axis",
                             choices = c( "baseMean",
                                          "log2FoldChange",
                                          "lfcSE",
                                          "stat",
                                          "pvalue",
                                          "padj"),
                             selected = "log2FoldChange"
                ),
                
                radioButtons(inputId = "ycol",
                             label = "Choose the column for the y-axis",
                             choices = c( "baseMean",
                                          "log2FoldChange",
                                          "lfcSE",
                                          "stat",
                                          "pvalue",
                                          "padj"),
                             selected = "padj"
                ),
                
                #Line break to add more space
                br(),
                
                p("Base point color"),
                
                colourInput("basecolour", label = NULL, value = "#22577A"),
                # Base point colourpicker here
                
                p("Highlight point color"),
                
                colourInput("highlightcolour", label = NULL, value = "#FFCF56"),
                # Highlight point colourpicker here
                
                p(),
                
                # Slider for the highlighting threshold ----
                sliderInput(inputId = "magnum",
                            label = "Select the magnitude of the p-adjusted coloring:",
                            min = -300,
                            max = 0,
                            value = -150),
                hr()
            ),
            
            # DEs plot panel
            mainPanel(
              tabsetPanel(
                # Diff Eq Result DataTable
                tabPanel("Table",
                         DT::dataTableOutput("deTable")
                ),
                
                # Differential Expression plot
                tabPanel("DE Plot",
                         # Volcano Plot
                         plotOutput(outputId = "volcano")
                         
                )
              )
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
        # extract data for summary columns
        column_name <- colnames(input_data)
        type <- sapply(input_data, class)
        # create summary of distinct values (dv)
        get_distinct_values <- function(df_column) {
            if (class(df_column) %in% c("character", "factor")) {
                dv <- as.factor(df_column)
                dv <- levels(dv)
            }
            else {
                colmean <- mean(df_column)
                colstdev <- sd(df_column)
                dv <- paste0(colmean, " (+/- ", colstdev, ")")
            }
            return(dv)
        }
        distinct_value <- sapply(input_data, get_distinct_values)
        
        # construct summary dataframe from columns
        summary_data_frame <- data.frame(column_name, type, distinct_value)
        # names(summary_data_frame) <- c("Column Name", "Type", "Mean (+/- sd) or Distinct Values")
        summary_data_frame
    }
    
    # Draw Volcano Plot in ggplot2
    #'
    #' @param dataf The loaded data frame.
    #' @param x_name The column name to plot on the x-axis
    #' @param y_name The column name to plot on the y-axis
    #' @param slider A negative integer value representing the magnitude of
    #' p-adjusted values to color. Most of our data will be between -1 and -300.
    #' @param color1 One of the colors for the points.
    #' @param color2 The other colors for the points. Hexadecimal strings: "#CDC4B5"
    #'
    #' @return A ggplot object of a volcano plot
    #'
    #' @examples volcano_plot(df, "log2fc", "padj", -100, "blue", "taupe")
    #' 
    volcano_plot <-
        function(dataf, x_name, y_name, slider, color1, color2) {
            highlight <- 10**slider
            # GGplot2 Volcano Plot
            volcano <- ggplot2::ggplot(dataf, mapping = ggplot2::aes(x = .data[[x_name]],
                                                   y = -log10(.data[[y_name]]),
                                                   color = (.data[[y_name]]) < highlight)
            ) + ggplot2::geom_point(
            ) + ggplot2::theme(legend.position = "bottom"
            ) + ggplot2::scale_color_manual(values = c(color1, color2)
            ) + ggplot2::labs(title="Plot of DE Results"
            ) + ggplot2::coord_fixed(ratio = 0.045)
            
            # volcano <- update_labels(volcano, list(x = x_name, y = y_name))
            
            return(volcano)
        }
    
    
    ## Output Elements go here
    
    # generate table
    
    #' generate a table and display its head (first 6 rows)
    output$countsTable <- renderTable({
        # table
        req(input$file)
        head(load_data())
    })
    
    #' generate summary table for sample summary tab
    output$summaryTable <- renderTable({
        # table
        req(input$sample_file)
        
        summary_table <- data_summary(load_sample())
        
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
    
    # Assignment RShiny Code
    
    output$volcano <- renderPlot({ # replace this NULL
        ##
        req(input$samplefile)
        
        return(
            volcano_plot(load_sample(), input$xcol, input$ycol, input$magnum, 
                         input$basecolour, input$highlightcolour)
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

