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

libs <- c("tidyverse", "ggVennDiagram", "BiocManager", "bigPint",
          "DESeq2", "edgeR", "limma",
          "gplots")
# if you don't have a package installed, use BiocManager::install() or 
# install.packages(), as previously discussed.
for (package in libs) {
    suppressPackageStartupMessages(require(package, 
                                           quietly = T, 
                                           character.only = T))
    require(package, character.only = T)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # UI Heading
   tags$head(
        # Application title
        titlePanel("TFogler RShiny Final Project"),
        h1("Soybean Cotyledon RNA-Seq Data Analysis")
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
                                   ".csv",
                                   "*.tsv"),
                        placeholder = "example_intensity_data_subset_69.csv"),
              
              selectInput(inputId = 'delim',
                          label = 'choose delimiter',
                          choices = c(',', '\t')),
              
              selectInput(inputId = 'column',
                          label = "Choose column to plot", 
                          choices = c(
                              'logFC',
                              'logCPM',
                              'LR',
                              'PValue',
                              'FDR'
                          ))
              
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
                    
                    # Datatable
                    p("Datatable of Samples Cols"),
                    
                    DT::dataTableOutput(outputId = 'sampleTable')
                ),
                
                tabPanel("Plots",
                    p("Plots of continuous variables"),
                    
                    #Plot here
                    plotOutput(outputId='samplePlots')
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
               fileInput("file", "Upreload *NORMALIZED* Counts Matrix File",
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
                           value = 0),
               
               # Input: Min Number of Non-zero Samples Slider
               sliderInput("zero",
                           "Minimum Number of Non-Zero Samples:",
                           min = 0,
                           max = 10,
                           value = 0)
           ),
           
           mainPanel(
              # Counts table panel
              tabsetPanel(
                  tabPanel("Summary",
                           # Summary table
                           p("Summary of Counts Filtering"),
                           
                           verbatimTextOutput("filteredSummaryText"),
                           
                           p("Counts Data Summary Table"),
                           
                           tableOutput(outputId = "summaryCountsTable")
                  ),
                  
                  tabPanel("Plots",
                      sidebarLayout(
                          sidebarPanel(
                              
                              # Base point colourpicker here
                              colourInput("pointColour", label = "Main Point Color", value = "#2D74A3"),
                              
                              # Filtered point colourpicker here
                              colourInput("filteredColour", label = "Filtered Point Color", value = "#112E42"),
                              
                          ),
                          
                          mainPanel(
                              p("Scatterplot of My Counts"),
                              
                              plotOutput(outputId = "scatterPlot", height = "360px")
                              
                          )
                      )
                  ),
                  
                  tabPanel("Heatmap",
                      p("Clustered Heatmap of Filtered Counts"),
                      
                      #Heatmap
                      plotOutput(outputId = "heatMap", height = '400px')
                  ),
                  
                  tabPanel("PCA",
                      p("Principal Component Analysis of Filtered Genes"),
                      
                      sidebarLayout(
                          sidebarPanel(
                              # Input: Principal Component X
                              selectInput(inputId = "pcx", label = "X Axis PC",
                                          choices = paste0(rep("PC", 9), seq(9)),
                                          selected = "PC1"),
                              
                              # Input: Principal Comp Y
                              selectInput(inputId = "pcy", label = "Y Axis PC",
                                          choices = paste0(rep("PC", 9), seq(9)),
                                          selected = "PC2")
                          ),
                          
                          mainPanel(
                            #PCA Plot
                            plotOutput(outputId = "pcaPlot", height = '360px')
                          )
                    )
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
                selectInput(
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
                         plotOutput(outputId = "volcano", height = "360px")
                         
                )
              )
            )
        ),
        
      ),
    )
)






# Define server logic #
server <- function(input, output) {
    ## Server reactives go here
    ############################################################################
    
    #' LOAD MY DATA SET REEEEE
    #' 
    re_get_soybean_cn <- reactive({
        data("se_soybean_cn_sub")
    })
    
    #' Get soybeans data
    #' 
    #' 
    reload_soybean_data <- reactive({
        # retrieve data
        re_get_soybean_cn()
        
        soybean_data <- assay(se_soybean_cn_sub) # SE Object-DelayedMatrix
        
        unfiltered_data <<- soybean_data %>% 
            as.data.frame() %>%
            mutate(gene = row.names(soybean_data), .before = 1)
        
        filtered <- unfiltered_data %>% filter_samples_zero_rows(input$zero) %>%
            filter_samples_var(input$var / 100)
        return(filtered)
    })

    
    reload_soybean_sample <- reactive({
        re_get_soybean_cn()
        rowData(se_soybean_cn_sub) %>% as.data.frame() # DFrame object by default
    })
    
    
    re_run_deseq <- reactive({
        #
        req(input$file)
        
        # get counts
        counts <- reload_data()
        counts <- as.data.frame(counts[-1], row.names = counts$gene)
        
        # make coldata
        coldata <- data.frame(condition = rep(c("day0", "day4", "day7", "adult"), each=2))
        row.names(coldata) <- names(counts)
        cat("coldata: ")
        print(coldata)
        
        # now run DESeq analysis
        deseq_res <<- run_deseq(counts, coldata, 10, "condition_day0_vs_adult")
    })
    
    # Choose DE analysis type
    de_analysis <- reactive({
        switch(input$de,
               "DESeq" = DESeq2,
               "limma" = limma,
               "edgeR" = edgeR)
    })
    
    # 
    
    #' (Re)-load in Counts Data from File
    #' 
    #' reloads data and returns dataframe
    reload_data <- reactive({
        req(input$file)
        f <- read.delim(input$file$datapath) #, row.names="gene")
        # f <- f[,c("vP0_1", "vP0_2", "vAd_1", "vAd_2")]
        unfiltered_data <<- f
        f <- f %>% filter_samples_zero_rows(input$zero) %>%
            filter_samples_var(input$var / 100)
        return(f)
    })
    
    reload_sample <- reactive({
        req(input$samplefile)
        return(read.csv(input$samplefile$datapath, sep = input$delim))
    })
    
    ## Server functions go here
    ############################################################################
    
    #' Filter Counts Data by Variance
    #' @param verse_counts counts data dataframe
    #' @param var_filter minimum quantile of variance [0-1.0]
    #' 
    #' @return Filtered counts with minimum percentile
    #' example filter_samples_var(verse_counts, input$variance)
    #' 
    filter_samples_var <- function(verse_counts, var_filter) {
        #this code does run.
        #~(but it's just really slow because
        #R really fkin hates doing row-wise operations)~
        # using apply syntax is smarter makes me more intelligent
        verse_counts_variance <- apply(verse_counts[, -1],
                              MARGIN = 1,
                              FUN = var,
                              na.rm = F
                        )
        
        min_var <- quantile(verse_counts_variance, var_filter)
        verse_counts <- verse_counts[verse_counts_variance >= min_var, ]
        
        return(verse_counts)
    }
    
    #' Filter Counts Data with Minimum Non-Zero Samples 
    #' filters out genes from counts by a minimum number 
    #' of nonzero samples, i.e. at least X samples in 
    #' gene are non-zero.
    #' 
    #' example filter_samples_zero_rows(verse_counts, 1)
    filter_samples_zero_rows <- function(verse_counts, min_nonzero) {
        # evaluate values where v is not zero
        is.zero <- \(v) (v == 0)
        nonzeros <- !apply(verse_counts[-1], c(1,2), is.zero)
        verse_counts$nonzeros <- apply(nonzeros, 1, sum)
        
        # select rows without zero
        verse_counts <- verse_counts %>%
            filter(nonzeros >= min_nonzero) %>%
            select(!matches('^nonzeros'))
        
        return(verse_counts)
    }
    
    #' Run DESeq on Counts Data.
    #' only run this one when selected.
    #' 
    #' @param count_dataframe The data frame of gene names and counts.
    #' @param count_filter An arbitrary number of genes each row should contain or 
    #' be excluded. DESeq2 suggests 10, but this could be customized while running. 
    #' An integer.
    #' @param condition_name A string identifying the comparison we are making. It 
    #' follows the format "condition_[]_vs_[]". If I wanted to compare day4 and day7 
    #' it would be "condition_day4_vs_day7".
    #'
    #' @return A dataframe of DESeq results. It has a header describing the 
    #' condition, and 6 columns with genes as row names. 
    #' @details This function is based on the DESeq2 User's Guide. These links describe 
    #' the inputs and process we are working with. The output we are looking for comes 
    #' from the DESeq2::results() function.
    #' 
    #' @examples run_deseq(counts_df, coldata, 10, "condition_day4_vs_day7")
    run_deseq <- function(counts_df, coldata, count_filter, condition_name) {
        
        # build DEseq dataset/se object from counts_df and coldata
        dds <- DESeqDataSetFromMatrix(
            countData = as.matrix(counts_df),
            colData = coldata,
            design = ~ condition
        )
        
        #filter out counts below filter threshold
        keep <- rowSums(counts(dds)) >= count_filter
        dds <- dds[keep,]
        
        #relevel factors in colDatat of dds
        #set "day0" to baseline
        dds$condition <- factor(dds$condition, levels = c(
            str_sub(condition_name, 11, 14),
            str_extract(condition_name, '[^_]+$')
        ))
        
        # run differential analysis
        deseq_res <- DESeq(dds)
        
        results(deseq_res)
    }
    
    #' 
    #' Summarize the data from input file.
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
                if (length(dv) > 6) {
                    #expression
                    # print('big pint')
                    dv <- c(dv[1:6], '...')
                }
            }
            else {
                colmean <- mean(df_column, na.rm = T)
                colstdev <- sd(df_column, na.rm = T)
                dv <- paste0(colmean, " (+/- ", colstdev, ")")
            }
            return(dv)
        }
        distinct_value <- sapply(input_data, get_distinct_values)
        distinct_value <- sapply(distinct_value, glue::glue_collapse, ", ")
        
        # construct summary dataframe from columns
        summary_df <- data.frame(column_name, type, row.names = NULL)
        # print(summary_df) #debug statement
        summary_df$distinct.value <- distinct_value
        names(summary_df) <- c("Column Name", "Type", "Mean (+/- sd) or Distinct Values")
        summary_df
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
            ) #+ ggplot2::coord_fixed(ratio = 0.045)
            
            # volcano <- update_labels(volcano, list(x = x_name, y = y_name))
            
            return(volcano)
        }
    


    #' Draw Scatterplot
    #' 
    #' Parses data behind the scenes and
    #' makes a GGPlot2 scatterplot of 
    #' Counts Data.
    #' 
    #' Diagnostic scatter plots,
    #' where genes passing filters are 
    #' marked in a darker color, and genes 
    #' filtered out are lighter.
    #' 
    #' One plot for Median Count vs log10(Variance)
    #' One for Median Count vs Number of Zeros    
    #' 
    counts_scatterplot <- function(counts_dataf, min_nonzero, var_filter, color1, color2) {
        # calculate median & var
        medians <- apply(counts_dataf[, -1], 1, median)
        variance <- apply(counts_dataf[, -1], 1, \(vec) (log10(var(vec) + 1)))
        
        # familiar xprs (see filter_samples functions)
        is.zero <- \(v) (v == 0)
        zeros <- apply(counts_dataf[-1], c(1,2), is.zero)
        total_zeros <- apply(zeros, 1, sum)
        
        # is the gene in the filtered dataframe?
        filtered <- counts_dataf %>%
                filter_samples_zero_rows(min_nonzero = min_nonzero) %>%
                filter_samples_var(var_filter = var_filter)
        kept_genes <- !(counts_dataf$gene %in% filtered$gene)
        
        # combine datar and then
        counts_summary <- data.frame(gene = counts_dataf$gene, medians, variance, total_zeros, kept_genes) %>%
                pivot_longer(cols = matches(c("zeros$", "variance$")), cols_vary = "fastest",
                             names_to = "variable", values_to = "X")
        
        # facet_wrap to combine two plots
        labeller = c(`total_zeros` = "Total Zeros", `variance` = "Log10 Variance")
        ggplot(counts_summary, aes(X, medians, color = kept_genes)
        ) + geom_point() + scale_color_manual(values = c(color1, color2)
        ) + facet_wrap(~ variable, labeller = as_labeller(labeller) # add descriptive labels
        ) + theme(legend.position = "none")
    }
    
    # Sample tab plots
    #
    draw_sample_plots <- function(sample_data, column) {
        # Data summary:
        # columns are logFC, logCPM, LR, PValue, FDR
        # then they go into early to middle to late 
        # S1-S2 S1-S3 S2-S3
        sample_data_longer <- pivot_longer(sample_data, c(-1)) %>%
            separate(name, into = c('group', 'v'), sep = '[.]') %>% #v for variable
            pivot_wider(names_from = v, values_from = value)
        # This *separate()s* the data into more ggplot-readable columns
        ggplot(sample_data_longer, mapping=aes(group, y=.data[[column]])) + geom_violin() +
            theme_light() %>%
        return()
    }
    
    # Counts PCA plot
    #
    plot_pca <- function(data, xcolumn, ycolumn, title="") {
        # no need to transpose data
        # data <- t(data)
        #compute pc's
        pca_results <- prcomp(data[-1])
        variances <- summary(pca_results)$importance[2,] # get prop of variance
                                                         # explained
        # get results
        principal.components <- pca_results$x %>% as.data.frame()
        
        # include variances in the labels
        variances <- glue::glue("({variances*100}% Var)") 
        labels <- paste(names(principal.components), variances) %>% 
            `names<-`(names(principal.components))
        # print(labels)
        # Draw PC Plot
        pc_plot <- ggplot(principal.components,
                          mapping=aes(.data[[xcolumn]], .data[[ycolumn]]),
                          labeller = as_labeller(labels)
            ) + geom_point() + labs(title = title) + xlab(labels[xcolumn]) + ylab(labels[ycolumn])
        
        return(pc_plot)
    }
    
    # Counts Heatmap
    #
    make_heatmap <- function(.data) {
        # make heatmap (stats)
        # not ggplot2
        marker_matrix <- as.matrix(
            select(.data, c("S1.1", "S1.2", "S1.3", "S2.1", "S2.2", "S2.3", "S3.1", "S3.2", "S3.3"))
        )
        rownames(marker_matrix) <- .data$gene
        
        heatmap.2(marker_matrix, col = cm.colors(231))
    }
    
    ## Output Elements go here
    ############################################################################
    
    #' Data Table for Samples Tab
    #' 
    output$sampleTable <- DT::renderDataTable({
        sample_info <- reload_soybean_sample()
        DT::datatable(sample_info, options = list(orderClasses = TRUE))
    })
    
    # Sample plots for Samples Plots Tab
    output$samplePlots <- renderPlot({
        draw_sample_plots(sample_data = reload_soybean_sample(),
                          column = input$column)
    })
    
    #' Filter summary
    #' 
    #' be sure to incl.:
    #'  number of samples
    #'  total number of genes
    #'  number and % of genes passing current filter
    #'  number and % of genes not passing current filter
    #' 
    output$filteredSummaryText <- renderText({
        # filtered # genes
        num_filtered_genes <- dim(reload_soybean_data())[1]
        # total # samples === # of numeric (int/float) rows
        num_samples <- sum(sapply(unfiltered_data, is.numeric))
        # total # of genes
        num_genes <- dim(unfiltered_data)[1]
        # calculate prop genes passing/not passing
        percent_filtered <- num_filtered_genes / num_genes
        
        filter_summary <- c("Total # of Samples: ", num_samples,
                            "Total # of Genes: ", num_genes,
                            " \nNumber of Genes Passing Current Filter: ",
                            num_filtered_genes,
                            "  \tPercentage of Genes Passing Filter: ",
                            percent_filtered * 100, " %",
                            " \nNumber of Genes NOT Passing Current Filter: ",
                            num_genes - num_filtered_genes,
                            "\tPercentage of Genes NOT Passing FIlter: ",
                            (1.0 - percent_filtered) * 100, " %",
                            sep = " ")
    })
    
    
    #' Generate Scatterplot in Scatterplot Tab
    #' 
    #' Tab with diagnostic scatter plots, 
    #' where genes passing filters are marked 
    #' in a darker color, and genes filtered 
    #' out are lighter:
    #' 
    output$scatterPlot <- renderPlot({
        # Create a plot of median values vs num zeros/variance
        # req(input$file)
        print(head(unfiltered_data))
        return(counts_scatterplot(unfiltered_data, input$zero, input$var / 100, 
                                  input$pointColour, input$filteredColour))
    })
    
    
    #' Heatmap Tab
    #' Draw Heatmap
    #' 
    #' Makes a heatmap based on counts data
    #' very simple block plotting filtered cts
    output$heatMap <- renderPlot({
        make_heatmap(reload_soybean_data())
    })
    
    #' PCA Tab
    #' 
    #' enters specified PC's from filtered cts on X and Y
    output$pcaPlot <- renderPlot({
        plot_pca(reload_soybean_data(), input$pcx, input$pcy, "Principal Components Plot of Counts")
    })
    
    #' Generate DESeq Results
    output$deResults <- renderTable({
        req(input$file)
        re_run_deseq()
        
        return(deseq_res)
    })
    
    #' generate table
    #' 
    #' generate a table and display its head (first 6 rows)
    output$countsTable <- renderTable({
        # table
        req(input$file)
        head(reload_soybean_data())
    })
    
    #' generate summary table for sample summary tab
    output$summaryTable <- renderTable({
        # table
        # req(input$samplefile)
        
        summary_table <- data_summary(reload_soybean_sample())
        summary_table <- as.data.frame(summary_table)
        print(summary_table) # debug statement
        return(summary_table)
    })
    
    #' summary table for counts summary tab
    output$summaryCountsTable <- renderTable({
        # table
        # req(input$file, cancelOutput = T)
        
        summary_table <- data_summary(reload_soybean_data())
        summary_table <- as.data.frame(summary_table)
        print(summary_table) # debug statement
        return(summary_table)
    })
    
    
    
    # diff_expr <- reload_sample()
    # output$deTable <- DT::renderDataTable({
    #     req(input$samplefile)
    #     DT::datatable(reload_sample(), options = list(orderClasses = TRUE))#, pageLength = 10)
    # })
    
    output$deTable <- DT::renderDataTable({
        req(input$file)
        re_run_deseq()
        deseq_res <<- data.frame(deseq_res)
        DT::datatable(deseq_res, options = list(orderClasses = TRUE))#, pageLength = 10)
    })
    
    
    # Assignment RShiny Code
    
    output$volcano <- renderPlot({
        ##
        req(input$file)
        
        return(
            volcano_plot(deseq_res, input$xcol, input$ycol, input$magnum, 
                         input$basecolour, input$highlightcolour)
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

