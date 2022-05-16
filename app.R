# Shiny App

### LIBRARIES 
########################### GENERAL PACKAGES
library(tidyr) # 
library(rjson) # Importing json files
library(purrr)
library(tidyverse)
library(lubridate)
library(furrr)
library(plotly) # for touchable plots
library(psych)
library(tictoc) # For timimg long functions
library(groupdata2) # For splitting data in the super-large lists
library(reactable)
library(stringr)

########################### TWEET COLLECTION: RETICULATE AND PYTHON PACKAGES

library(tidyverse)
library(reticulate)
### These are the python packages I installed using the package.
# reticulate::py_install("pandas")
# reticulate::py_install("tweepy")
# reticulate::py_install("numpy")
# reticulate::py_install("time")

########################### TEXT ANALYSIS: QUANTEDA SUITE
library(quanteda)
library(quanteda.dictionaries) # includes liwcalike function
library(quanteda.corpora) # Datasets & tutorials for quanteda
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(tm)

# Used for reading in text
library(readtext)

########################### SHINY-SPECIFIC PACKAGES
# These packages will also be called in Shiny, but I figure it's useful to them here as well
library(thematic)
library(bslib)
library(shiny)


########################## LOADING DATA
load("shiny_data.RData")

fig_data <- ws %>% 
  dplyr::group_by(round_date(tweet_date, 
                             unit = "day")) %>% 
  summarise(.groups = 'keep',
            ntweet = n()) %>% 
  as_tibble() 

names(fig_data)[1] <- "tweet_date"



########################## UI


ui <- fluidPage(
  
  navbarPage(
    title = "A Deep Dive into Cancel Culture",
    tabPanel(
      title = "Timeline of Tweets Cancelling Will Smith",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          radioButtons(
            inputId = "verified",
            label = "See Analysis of Tweets By:",
            choices = c("Verified Users", "Unverified Users", "All Users"),
            selected = "Verified Users",
            inline = TRUE
          )
        ),
        mainPanel(
          plotlyOutput("distPlot_ws", height = "800px")
        )
      )
    ),
    
    tabPanel(
      title = "Cancel Culture",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          radioButtons(
            inputId = "y_axis",
            label = "What kind of language should we code for?",
            choices = c("negative", "positive", "valence"),
            selected = "negative",
            inline = TRUE
          ),
          radioButtons(
            inputId = "x_axis",
            label = "Types of Twitter Users",
            choices = c("followers_count", "following_count", "tweet_count"),
            selected = "followers_count",
            inline = TRUE
          ),
        ),
        mainPanel(
          plotlyOutput("distPlot_cc", height = "800px")
        )
      )
    )
    
  )
)


########################## SERVER
server <- function(input, output, session) {
  
  ###### THE REACTIVE FUNCTIONS ARE NOT UP AND RUNNING YET, THEY'RE TOO COMPUTATIONALLY INTENSIVE, SO I'M DIGGING INTO HOW TO DO THEM FASTER 
  #   fig_data <- #reactive({ ws %>% 
  #     # Filtering by input
  #     if(stringr::str_detect(input$verified, "Verified Users")){
  #         filter(ws$verified == TRUE)}
  #       else if(stringr::str_detect(input$verified, "Unverified Users")){
  #         filter(ws$verified == FALSE)}
  #       else{} %>% 
  #     # Grouping to get nTweets per day
  #     fig_data <- ws %>% 
  #         dplyr::group_by(round_date(tweet_date, 
  #                                    unit = "day")) %>% 
  #         summarise(.groups = 'keep',
  #                   ntweet = n()) %>% 
  #         as_tibble() 

  output$distPlot_ws <- renderPlotly({
  ############ PLOTLY CODE EXAMPLE
   fig <- plot_ly(fig_data, 
                  name = "Timeline of Tweets Cancelling Will Smith",
                  x = ~tweet_date, 
                  y = ~ntweet, 
                  type = 'scatter', 
                  mode = 'lines+markers') 
   fig <- fig %>% layout(title = "Timeline of Tweets Cancelling Will Smith",
                          paper_bgcolor='rgb(255,255,255)', 
                          plot_bgcolor='rgb(229,229,229)',
                          xaxis = list(title = "Date",
                                      gridcolor = 'rgb(255,255,255)',
                                      showgrid = TRUE,
                                      showline = FALSE,
                                      showticklabels = TRUE,
                                      tickcolor = 'rgb(127,127,127)',
                                      ticks = 'outside',
                                      zeroline = FALSE),
                         yaxis = list(title = "Number of Tweets",
                                      gridcolor = 'rgb(255,255,255)',
                                      showgrid = TRUE,
                                      showline = FALSE,
                                      showticklabels = TRUE,
                                      tickcolor = 'rgb(127,127,127)',
                                      ticks = 'outside',
                                      zeroline = FALSE))
   fig
  })
  
  output$distPlot_cc <- renderPlotly({
    fig <- plot_ly(cc, 
                   name = "What do different types of Twitter Users think about Cancel Culture?",
                   x = ~followers_count, 
                   y = ~negative, 
                   type = 'scatter', 
                   mode = 'markers') 
    fig <- fig %>% layout(title = "What do different types of Twitter Users think about Cancel Culture?",
                          paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                          xaxis = list(title = "followers_count",
                                       gridcolor = 'rgb(255,255,255)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(127,127,127)',
                                       ticks = 'outside',
                                       zeroline = FALSE),
                          yaxis = list(title = "negative",
                                       gridcolor = 'rgb(255,255,255)',
                                       showgrid = TRUE,
                                       showline = FALSE,
                                       showticklabels = TRUE,
                                       tickcolor = 'rgb(127,127,127)',
                                       ticks = 'outside',
                                       zeroline = FALSE))
    fig
    
   # # Aagain, alas, the selectable inputs aren't working :(
   #df <- tibble(
   #  xx = cc %>% select(sym(input$x_axis)),
   #  yy = cc %>% select(sym(input$y_axis))
   #)
   #fig <- plot_ly(df, 
   #               name = "What do different types of Twitter Users think about Cancel Culture?",
   #               x = ~xx, 
   #               y = ~yy, 
   #               type = 'scatter', 
   #               mode = 'lines+markers') 
   #fig <- fig %>% layout(title = "What do different types of Twitter Users think about Cancel Culture?",
   #                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
   #                      xaxis = list(title = paste0(input$x_axis),
   #                                   gridcolor = 'rgb(255,255,255)',
   #                                   showgrid = TRUE,
   #                                   showline = FALSE,
   #                                   showticklabels = TRUE,
   #                                   tickcolor = 'rgb(127,127,127)',
   #                                   ticks = 'outside',
   #                                   zeroline = FALSE),
   #                      yaxis = list(title = paste0(input$y_axis),
   #                                   gridcolor = 'rgb(255,255,255)',
   #                                   showgrid = TRUE,
   #                                   showline = FALSE,
   #                                   showticklabels = TRUE,
   #                                   tickcolor = 'rgb(127,127,127)',
   #                                   ticks = 'outside',
   #                                   zeroline = FALSE))
   #fig
  })
}
########################## RUN
shinyApp(ui = ui, server = server)


































