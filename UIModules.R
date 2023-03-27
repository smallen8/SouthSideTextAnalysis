WordCountUI <- function(id = NULL) {
  
  layout <- sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 3,
      selectInput(inputId = 'groupByVar',
                  label = 'Summarize By',
                  choices = list("Don't Summarize" = "Don't Summarize",
                                 'Age Group' = 'age_group',
                                 'Has Kids' = 'has_kids',
                                 'Life Stage' = 'stage_of_life'),
                  selected = "Don't Summarize"),
      sliderInput(inputId = 'myN',
                  label = 'Top N Words',
                  value = 10,
                  min = 1,
                  max = 20,
                  step = 1)
    ),
    mainPanel = mainPanel(
      width = 9,
      plotOutput('plotTopNWords'),
      br(), br(),
      plotOutput('plotTopNBigrams'),
      br(), br(),
      plotOutput('plotTopNTrigrams')
    )
  )
}

WordCorrelationUI <- function(id = NULL) {
  layout <- sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 3,
      selectInput(inputId = 'corrWord',
                  label = 'Word',
                  choices = sort(topWordList),
                  selected = sort(topWordList)[5]),
      sliderInput(inputId = 'myN2',
                  label = 'Top N Words',
                  value = 5,
                  min = 1,
                  max = 20,
                  step = 1),
      sliderInput(inputId = 'myCorr',
                  label = 'Correlation Cutoff',
                  value = .3,
                  min = 0,
                  max = 1,
                  step = .1)
    ),
    mainPanel = mainPanel(
      width = 9,
      h3(glue::glue("Words correlated with chosen word:")),
      dataTableOutput('getCorrelatedWords'),
      br(), br(),
      h3(glue::glue('Words commonly appearing in the same sentence:')),
      plotOutput('plotWordMap')
    )
  )
}

ResponsesUI <- function(id = NULL) {
  fluidPage(
    dataTableOutput('displayRawText')
  )
}
