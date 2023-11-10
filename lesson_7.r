df <- read.csv("ДЗ2_vgsales.csv")
library(shiny)
library(dplyr)
library(ggplot2)
print(head(df))

df %>%
group_by(NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>%
summarise(mean = mean(NA_Sales), .groups = 'drop_last')

linear_plot(df, 'Year', c('NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales'))


linear_plot(df, 'Year', c('NA_Sales', 'EU_Sales', 'JP_Sales', 'Other_Sales'))
bar_plot(df, 'Genre', 'NA_Sales')
bar_plot(df, 'Genre', 'EU_Sales')
bar_plot(df, 'Genre', 'JP_Sales')
bar_plot(df, 'Genre', 'Other_Sales')

observe({
    req(input$platform)
    filtered_df <- df %>% filter(Platform %in% input$platform)
    output$table <- renderTable({
        filtered_df
    })
})

observeEvent(input$yearrange, {
    min_year <- input$yearrange[1]
    max_year <- input$yearrange[2]
    filtered_data <- reactive({
        df %>%
        filter(Year >= min_year & Year <= max_year)
    })
    output$table <- renderTable({
        filtered_data()
    })
})