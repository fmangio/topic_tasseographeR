
#Load required packages
library(shiny)
library(quanteda)
library(dplyr)
library(ggplot2)
library(quanteda.sentiment)
library(quanteda.dictionaries)
library(tardis)
library(shinythemes)

library(readr)


# Increase the maximum upload size
options(shiny.maxRequestSize = 100*1024^2)


# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Sleek theme
  titlePanel("Topic Function TasseographeR"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Adjust sidebar width for better space management 
      fileInput("file", "Choose CSV File (ds)"),
      selectInput("dictionary", "Select Dictionary:", 
                  choices = c("User Defined", "Uploaded Dictionary"), 
                  selected = "User Defined"),
      textInput("user_dictionary", "Enter User Defined Dictionary (comma-separated):"),
      fileInput("dictionary_file", "Upload Dictionary (CSV):"),
      downloadButton("downloadPlot", "Download PDF"),
      downloadButton("downloadCSV", "Download CSV"),  # New button for CSV download
      # Citation Block
      div(class = "citation-block",
          h4("Preferred Citation:"),
          p("Mangiò, F. (2025). Topic TasseographeR: A web-app for topic modeling interpretation.", style = "font-style: italic;")
      )
    ),
    mainPanel(
      plotOutput("topicPlot")
    )
  )
)


# Server
server <- function(input, output) {
  
  # Reactive function to process the dictionary
  getDictionary <- reactive({
    if (input$dictionary == "User Defined") {
      user_defined_dictionary <- unlist(strsplit(input$user_dictionary, ",\\s*"))
      return(dictionary(list(dictionary_lemmas = user_defined_dictionary)))
    } else if (input$dictionary == "Uploaded Dictionary") {
      req(input$dictionary_file)
      uploaded_dict <- read.csv(input$dictionary_file$datapath, header = TRUE)
      
      if (!"dictionary_lemmas" %in% names(uploaded_dict)) {
        stop("Uploaded dictionary must contain a 'dictionary_lemmas' column.")
      }
      
      return(dictionary(list(dictionary_lemmas = uploaded_dict$dictionary_lemmas)))
    } else {
      stop("Invalid dictionary selection.")
    }
  })
  
  # Reactive data for plot and CSV
  plotData <- reactive({
    req(input$file)  # Ensure a file is uploaded
    
    # Load the CSV file
    ds <- read_delim(input$file$datapath, delim = ";", escape_double = FALSE, trim_ws = TRUE)
    
    # Validate if ds is a data frame
    if (!is.data.frame(ds)) {
      stop("Uploaded file is not a valid CSV data frame.")
    }
    
    # Ensure 'Text' column exists
    if (!"Text" %in% names(ds)) {
      stop("The uploaded CSV file must contain a 'Text' column.")
    }
    
    # Remove NAs from ds
    ds_no_na <- ds[complete.cases(ds), ]
    
    # Apply the selected dictionary
    dictionary <- getDictionary()
    
    # Apply dictionary to ds$Text
    dfmat <- dfm(tokens(ds_no_na$Text))
    
    # Validate dfmat
    if (nfeat(dfmat) == 0) {
      stop("The tokenized data frame is empty. Check the 'Text' column content.")
    }
    
    dic_glob <- convert(dfm_lookup(dfmat, dictionary, valuetype = "glob", exclusive = TRUE, capkeys = TRUE), to = "data.frame")
    
    # Validate dic_glob
    if (nrow(dic_glob) == 0) {
      stop("Dictionary lookup returned no matches. Check your dictionary and text data.")
    }
    
    # Get original metadata
    dic_glob$doc_id <- as.factor(dic_glob$doc_id)
    ds_no_na$doc_id <- as.factor(paste("text", 1:nrow(ds_no_na), sep = ""))
    
    top_docs_df_merged <- merge(ds_no_na, dic_glob, by = "doc_id")
    
    # Create weighted score
    for (topic_col in grep("^Topic[0-9]+$", names(top_docs_df_merged), value = TRUE)) {
      weighted_score_col <- paste0("Weighted_", topic_col, "_score")
      top_docs_df_merged[[weighted_score_col]] <- as.numeric(top_docs_df_merged$DICTIONARY_LEMMAS) * as.numeric(top_docs_df_merged[[topic_col]])
    }
    
    weighted_topic_score_columns <- grep("^Weighted_Topic[0-9]+_score$", names(top_docs_df_merged), value = TRUE)
    
    # Calculate the sum of weighted topic scores across all documents
    sum_weighted_topic_scores <- colSums(top_docs_df_merged[weighted_topic_score_columns], na.rm = TRUE)
    
    # Create a data frame for plotting
    data.frame(Topic = sub("^Weighted_Topic([0-9]+)_score$", "\\1", names(sum_weighted_topic_scores)),
               Sum_Score = sum_weighted_topic_scores)
  })
  
  # Render the plot
  output$topicPlot <- renderPlot({
    sum_data <- plotData()
    
    ggplot(sum_data, aes(x = reorder(Topic, -Sum_Score), y = Sum_Score)) +
      geom_bar(stat = "identity", fill = "brown", color = "black", alpha = 0.7) +
      labs(title = "Topic Function Score",
           x = "Topic",
           y = "Topic Function Score") +
      theme_minimal(base_size = 14) +  # Set base font size
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      ) +
      coord_flip()
  })
  
  # Download PDF button
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("weighted_topic_scores_plot", ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 10, height = 6)
      sum_data <- plotData()
      ggplot(sum_data, aes(x = reorder(Topic, -Sum_Score), y = Sum_Score)) +
        geom_bar(stat = "identity", fill = "brown", color = "black", alpha = 0.7) +
        labs(title = "Topic Function Score",
             x = "Topic",
             y = "Topic Function Score") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_flip()
      dev.off()
    }
  )
  
  # Download CSV button
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("topic_function_scores", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(plotData(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)