# Objective: build a bank churn UI

library(tidyverse)
library(shiny)            # create shiny app
library(shinythemes)      # shiny themes
library(caret)            # streamline the model training process for complex regression and classification problems
library(randomForest)     # random forest model
library(ModelMetrics)     # F1 Score

# Read in the model
model <- readRDS("../codes/rf_model.rds")

train <- read_csv("../data/processed/train.csv")
cols_factor <- c("rim_status_closed", "type")
train[cols_factor] <- lapply(train[cols_factor], as.factor)
test_input <- read_csv("../data/processed/test_input.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Customer Churn Predictions"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "upload csv file here",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")), 
      
      
      # Button
      downloadButton("downloadData", "Download the Predictions")
    ),
    # Show the table with the predictions
    mainPanel(
      DT::dataTableOutput("mytable")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  reactiveDF<-reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = TRUE)
    cols_factor <- c("type")
    test_input[cols_factor] <- lapply(test_input[cols_factor], as.factor) 
    
    drop_cols = c("CRE", "HE", "LOC", "cur_bal", "orig_amt", "online_duration", 
                  "missing_payment", "avg_bal", "avg_rate", "total_amt", "total_ct", "avail_bal",
                  "mortgage_rating", "email_rating", "member_id")
    test_input2 <- test_input %>% dplyr::select(-all_of(drop_cols))
    test_input2 <- rbind(train[1, -1] , test_input2)
    test_input2 <- test_input2[-1,]
    

    pred_prob<-predict(model, newdata = test_input2, type ="prob")
    Output <- round(pred_prob, 3)[,2]
    df$churn_prob <- Output
    
    df <- df %>% mutate(churn_when = case_when(churn_prob < 0.5 ~ "Less likely to churn within 3 months",
                                               churn_prob >= 0.5 ~"Likely to churn within 3 months"
                                               )) 
    
    return(df)
    
  })
  
  output$mytable = DT::renderDataTable({
    req(input$file1)
    
    return(DT::datatable(reactiveDF(),  options = list(pageLength = 100), filter = c("top")))
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(reactiveDF(), file, row.names = FALSE)
    }
  )
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)


# References:
# https://shiny.rstudio.com/reference/shiny/1.6.0/fileInput.html
# https://mastering-shiny.org/basic-app.html
# https://www.youtube.com/watch?v=9uFQECk30kA