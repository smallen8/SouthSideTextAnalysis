#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(data.table)
library(reactable)
library(htmltools)

library(dplyr)
library(tidyr)
library(widyr)

library(tidytext)
data("stop_words")

library(igraph)
library(ggraph)
library(ggplot2)

theme_set(theme_minimal())
colorPalette <<- 'Blues'
textSize <<- 15

set.seed(2022)

source('ViolenceDiscussion.R')
source('UIModules.R')

# Load data.
text_df <<- loadData()

# Categorical variables in metadata that can be used for grouping later
summarizeVars <<- c("Don't Summarize", 'age_group', 'stage_of_life', 'has_kids')
topWordList <<- countWords(text_df)$word[1:20]

LaunchUI <- tryCatch({
  
  source('ViolenceDiscussion.R')
  source('UIModules.R')
  text_df <<- loadData()
  summarizeVars <<- c("Don't Summarize", 'age_group', 'stage_of_life', 'has_kids')
  topWordList <<- countWords(text_df)$word[1:20]
  
  ui <- div(fluidPage(
    navbarPage(id = 'Main',
               title = div(icon('hot-tub', lib='font-awesome'), 'South Side Flats - Neighbor Feedback'),
               tabPanel(title = 'Frequent Phrases', value = 'Frequent Phrases', WordCountUI()),
               tabPanel(title = 'Correlated Words', value = 'Correlated Words', WordCorrelationUI()),
               tabPanel(title = 'Neighbor Feedback', value = 'Neighbor Feedback', ResponsesUI())
    )
  ))
  
})

LaunchServer <- tryCatch({
  
  server <- function(input, output, session) {
    
    source('ViolenceDiscussion.R')
    source('UIModules.R')
    text_df <<- loadData()
    summarizeVars <<- c("Don't Summarize", 'age_group', 'stage_of_life', 'has_kids')
    topWordList <<- countWords(text_df)$word[1:20]
    
    output$plotTopNWords <- renderPlot({
      plotTopWords(text_df, groupByVar = input$groupByVar, tokenType = 'word', myN = input$myN)
    })
    output$plotTopNBigrams <- renderPlot({
      plotTopWords(text_df, groupByVar = input$groupByVar, tokenType = 'bigram', myN = input$myN)
    })
    output$plotTopNTrigrams <- renderPlot({
      plotTopWords(text_df, groupByVar = input$groupByVar, tokenType = 'trigram', myN = input$myN)
    })
    
    output$getCorrelatedWords <- renderDataTable({
      getTopCorrelatedWords(text_df, myN = input$myN2, myCorr = input$myCorr,
                            myWords = input$corrWord)
    })
    output$plotWordMap <- renderPlot({
      plotWordCorrGraph(text_df, myCorr = input$myCorr)
    })
    # output$getCorrelatedWords2 <- reactableOutput({
    #     raw_text <- read.csv('~/Documents/SouthSideFlats/text.csv')
    #     raw_text <- raw_text %>%
    #         filter(!is.na(text), trimws(text)!='')
    #    
    #     reactable(
    #         df = getTopCorrelatedWords(text_df, myN = input$myN2, myCorr = input$myCorr,
    #                                    myWords = input$corrWord),
    #         details = function(index) {
    #             nestedData <- data.frame(Details = raw_text[intersect(grep(word1[index], raw_text$text),
    #                                                                   grep(word2[index], raw_text$text)),])
    #             htmltools::div(style = 'padding: 16px',
    #                            reactable(nestedData, outlined = T))
    #         }
    #     )
    # })
    
    output$displayRawText <- renderDataTable({
      raw_text <- read.csv('text.csv')
      raw_text$neighbor_not_anonymous <- NULL
      raw_text <- raw_text %>%
        filter(!is.na(text), trimws(text)!='') %>%
        group_by(neighbor) %>%
        summarise_all(toString)
      
      datatable(raw_text,
                options = list(searchHighlight = T))
    })
    
  }
})

shinyApp(ui = ui, server = server)