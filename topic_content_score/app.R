# Increase the maximum upload size
options(shiny.maxRequestSize = 50*1024^2)

library(shiny)
library(quanteda)
library(dplyr)
library(ggplot2)
library(shinythemes)


# Define the UI with improved aesthetics
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Sleek theme
  
  titlePanel("Topic Content TasseographeR"),
  
  # Sidebar layout with responsive design
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Adjust sidebar width for better space management
      
      # File input for td_beta CSV file
      fileInput("td_betaFile", "Upload td_beta CSV File:", accept = c(".csv")),
      
      # Select input for choosing sentiment dictionary
      selectInput("dictionary", "Select Dictionary:", 
                  choices = c("User Defined Dictionary" = "user_dictionary", 
                              "Uploaded Dictionary" = "uploaded_dictionary"), 
                  selected = "user_dictionary"),
      
      # Conditional input for user-defined or uploaded dictionaries
      conditionalPanel(
        condition = "input.dictionary == 'user_dictionary'",
        textInput("userDictionaryInput", "Enter user dictionary lemmas (comma-separated):", ""),
        actionButton("submitUserDictionary", "Submit", class = "btn-primary")
      ),
      
      conditionalPanel(
        condition = "input.dictionary == 'uploaded_dictionary'",
        fileInput("dictionaryFile", "Upload Dictionary CSV File:", accept = c(".csv"))
      ),
      
      # Download buttons
      br(),
      downloadButton("downloadPlot", "Download Plot (PDF)", class = "btn-success"),
      downloadButton("downloadScores", "Download Scores (CSV)", class = "btn-primary"),
      
      # Citation Block
      div(class = "citation-block",
          h4("Preferred Citation:"),
          p("Mangiò, F. (2025). Topic TasseographeR: A web-app for topic modeling interpretation.", style = "font-style: italic;")
      )
    ),
    
    # Main panel with plot output
    mainPanel(
      width = 9,  # Adjust main panel width
      plotOutput("topicPlot", height = "600px")  # Make the plot a bit taller
    )
  )
)

# Define the Server
server <- function(input, output) {
  
  # Reactive function to read the uploaded CSV file for td_beta
  td_beta <- reactive({
    req(input$td_betaFile)
    inFile <- input$td_betaFile
    data <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    
    # Ensure required columns exist
    if (!all(c("term", "beta", "topic") %in% colnames(data))) {
      stop("The uploaded file must contain 'term', 'beta', and 'topic' columns.")
    }
    
    # Convert term to character
    data$term <- as.character(data$term)
    return(data)
  })
  
  # Reactive function to read the uploaded dictionary CSV file
  uploaded_dictionary <- reactive({
    req(input$dictionaryFile)
    dictFile <- input$dictionaryFile
    dictData <- read.csv(dictFile$datapath, stringsAsFactors = FALSE)
    
    # Ensure the required column exists
    if (!"term" %in% colnames(dictData)) {
      stop("The uploaded dictionary file must contain a column named 'term'.")
    }
    
    # Extract terms
    as.character(dictData$term)
  })
  
  # Reactive function to get the selected dictionary
  selected_dictionary <- reactive({
    if (input$dictionary == "user_dictionary") {
      if (!is.null(input$userDictionaryInput)) {
        strsplit(input$userDictionaryInput, ",")[[1]]
      } else {
        character(0)
      }
    } else if (input$dictionary == "uploaded_dictionary") {
      uploaded_dictionary()
    }
  })
  
  # Reactive function to update the plot based on selected dictionary
  scores_data <- reactive({
    # Ensure the selected dictionary is available
    dictionary_terms <- selected_dictionary()
    if (is.null(dictionary_terms) || length(dictionary_terms) == 0) {
      stop("No valid dictionary selected or uploaded.")
    }
    
    # Build dictionary
    dictionary <- dictionary(list(dictionary_lemmas = dictionary_terms)) 
    
    # Apply dictionary to td_beta
    dfmat <- dfm(tokens(td_beta()$term)) 
    dic_glob <- dfm_lookup(dfmat, dictionary, valuetype = "glob", exclusive = TRUE, capkeys = TRUE)
    
    # Get original metadata
    dic_glob$doc_id <- as.factor(docnames(dfmat))
    
    td_beta_merged <- cbind(td_beta(), convert(dic_glob, to = "data.frame"))
    
    # Rename columns if there are duplicates
    colnames(td_beta_merged) <- make.names(colnames(td_beta_merged), unique = TRUE)
    
    # Create weighted score
    td_beta_merged <- td_beta_merged %>% 
      mutate(total_score = (DICTIONARY_LEMMAS * beta * 100)) %>%
      group_by(topic) %>%
      summarise(score = sum(total_score)) 
    
    return(td_beta_merged)
  })
  
  output$topicPlot <- renderPlot({
    td_beta_merged <- scores_data()
    
    # Create a bar plot
    ggplot(td_beta_merged, aes(x = factor(topic), y = score)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Topic Content Scores",
           x = "Topic",
           y = "Topic Content Score") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
      ) +
      coord_flip()
  })
  
  # Download the saved plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot", Sys.Date(), ".pdf", sep = "_")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf", width = 10, height = 6)
    }
  )
  
  # Download the scores as CSV
  output$downloadScores <- downloadHandler(
    filename = function() {
      paste("topic_scores", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(scores_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)



