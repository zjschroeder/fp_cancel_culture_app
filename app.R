# Shiny App

### LIBRARIES 
########################### GENERAL PACKAGES
library(purrr)
library(tidyverse)
library(plotly) # for touchable plots
library(groupdata2) # For splitting data in the super-large lists
library(stringr)
library(lubridate)
library(shiny)

########################## LOADING DATA
load("shiny_data.RData")
d <- willsmith_deidentified_analyzed

fig_data <- willsmith_deidentified_analyzed %>% 
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
            inline = FALSE
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
            label = "Tweet Content",
            choices = c("followers_count", "following_count", "tweet_count"),
            selected = "followers_count",
            inline = FALSE
          ),
          radioButtons(
            inputId = "x_axis",
            label = "Tweet Variables",
            choices = c("like_count", "reply_count", "retweet_count"),
            selected = "like_count",
            inline = FALSE
          ),
          radioButtons(
            inputId = "verified",
            label = "Is Verified?",
            choices = c("Yes", "No"),
            selected = "No"
          )
        ),
        mainPanel(
          plotOutput("distPlot_cc", height = "800px")
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
   fig <- plot_ly(fig_data, 
                  name = "Timeline of Tweets Cancelling Will Smith",
                  x = ~tweet_date, 
                  y = ~ntweet, 
                  type = 'scatter', 
                  mode = 'lines+markers') 
   fig %>% layout(title = "Timeline of Tweets Cancelling Will Smith",
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
  })
  
  
  output$distPlot_cc <- renderPlot({
    willsmith_deidentified_analyzed %>% 
      ggplot(aes(x = scale(!!sym(input$x_axis)),
                 y = scale(!!sym(input$y_axis)))) +
         geom_point()
  })
}
########################## RUN
shinyApp(ui = ui, server = server)


#### SHINY PROFILER AND PROFILING CODE; arrow package

























