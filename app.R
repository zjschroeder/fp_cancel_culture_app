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
#load("shiny_data.RData")
load("shiny_data_DI.RData")

fig_data <- list(NULL)

for(i in seq_along(analyzed_DI)){
  fig_data[[i]] <- analyzed_DI[[i]] %>% 
    dplyr::group_by(round_date(tweet_date, 
                               unit = "week")) %>% 
    summarise(.groups = 'keep',
              ntweet = n()) %>% 
    as_tibble() 
  names(fig_data[[i]]) <- c("tweet_date", "ntweet")
}

names(fig_data) <- names(analyzed_DI)

targets <- c("BBQ Becky", 
             "Justine Sacco", 
             "Permit Patty", 
             "Kanye West", 
             "Lindsay Ellis", 
             "R. Kelly", 
             "Will Smith", 
             "Doja Cat")

######### Map_dbl negativity correlations

negativity_retweets_cor <- map(analyzed_DI, ~split(.x, .x$verified) %>% 
  map_dbl( ~cor(.x$retweet_count, .x$negative))) 

negativity_correlations <- unlist(negativity_retweets_cor) %>% 
  round(., 2) %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  as_tibble()

negativity_correlations <- negativity_correlations %>% 
  mutate(
    targets = targets
  )

names(negativity_correlations)  <- c("Not Verified", "Verified", "Cancel-ee")

######### purrr::nest %>% mutate() %>% unnest()
data <- analyzed_DI$bbqbecky

engagement <- data %>% 
  group_by(negative) %>% 
  nest() %>% 
  mutate(map(.x = data, .f = ~mean(.x$retweet_count, na.rm = T)))

########################## UI

ui <- fluidPage(
  navbarPage(
    title = "A Deep Dive into Cancel Culture",
    tabPanel(
      title = "Timeline of Cancelling Tweeets",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput(
            inputId = "target",
            label = "See Analysis of Tweets Cancelling:",
            choices = c("BBQ Becky" = "1", 
                        "Justine Sacco" = "2", 
                        "Permit Patty" = "3", 
                        "Kanye West" = "4", 
                        "Lindsay Ellis" = "5", 
                        "R. Kelly" = "6", 
                        "Will Smith" = "7", 
                        "Doja Cat" = "8"),
            selected = "BBQ Becky"
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
          selectInput(
            inputId = "target_cc",
            label = "See Analysis of Tweets Cancelling:",
            choices = c("BBQ Becky" = "1", 
                        "Justine Sacco" = "2", 
                        "Permit Patty" = "3", 
                        "Kanye West" = "4", 
                        "Lindsay Ellis" = "5", 
                        "R. Kelly" = "6", 
                        "Will Smith" = "7", 
                        "Doja Cat" = "8"),
            selected = "BBQ Becky"
          ),
          radioButtons(
            inputId = "y_axis",
            label = "Tweet Content",
            choices = c("MindOverall", "negative", "positive"),
            selected = "MindOverall",
            inline = FALSE
          ),
          radioButtons(
            inputId = "x_axis",
            label = "Tweet Variables",
            choices = c("MindOverall", "negative", "positive"),
            selected = "negative",
            inline = FALSE
          )),
        mainPanel(
          plotOutput("distPlot_cc", height = "800px")
        )
      )
    )
  )
)




########################## SERVER
server <- function(input, output, session) {
  
  outVar <- reactive({
    temp <- fig_data[[as.numeric(input$target)]]
  })
  
  output$distPlot_ws <- renderPlotly({
    fig <- plot_ly(outVar(), 
                   name = "Timeline of Tweets Cancelling Will Smith",
                   x = ~tweet_date, 
                   y = ~ntweet, 
                   type = 'scatter', 
                   mode = 'lines+markers') 
    fig %>% layout(title = paste("Timeline of Tweets Cancelling", targets[as.numeric(input$target)]),
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
  outVarcc <- reactive({
    temp <- analyzed_DI[[as.numeric(input$target_cc)]]
  })
  
  output$distPlot_cc <- renderPlot({
    ggplot(outVarcc(),
           aes(x = !!sym(input$x_axis),
               y = !!sym(input$y_axis))) +
      geom_point() +
      geom_jitter() +
      geom_smooth()
  })
}

########################## RUN
shinyApp(ui = ui, server = server)


#### SHINY PROFILER AND PROFILING CODE; arrow package



